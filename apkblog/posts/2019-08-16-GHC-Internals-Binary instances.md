---
title: GHC Internals - Binary instances, Interface files and number encodings.
tags: Haskell, GHC, optimizations.
---

GHC uses it's own Binary class for a hand full of reasons.
These in turn are used to serialize various values
This class and it's instances are used to write out `.hi` (interface) files.

They are used to share information about already compiled modules
eg function definitions for inlining.

I recently [changed](https://gitlab.haskell.org/ghc/ghc/merge_requests/1536) the implementation for the Integer instance which
up to then encoded large integers as UTF32-Strings.

As a follow up I looked at implementing something similar for Int[64/32/16] using [LEB128](https://en.wikipedia.org/wiki/LEB128).

## What is LEB128

It's a variable length encoding for numbers.  
This means if our number will fit in 7 bits, we use at most a byte.

The basic principle is that in each byte:
* We use 7 bits for data.
* One bit to inform as if we need to read more data.

While 1/8th overhead might seem like a lot in practice most numbers,
even for Int64, are small values. In fact zero is the most common number.

So while the worst case is worse (10 bytes for Int64) on average save a lot
of space. Saving 7 bytes for any value fitting into 7 bits.

## The implementation

There are two versions of LEB128, signed and unsigned.  
These are straight forward translations from the algorithm given on wikipedia,
and are not yet very optimized.

### Unsigned

The unsigned one is rather straight forward:

```haskell
putULEB128 :: forall a. (Integral a, FiniteBits a) => BinHandle -> a -> IO ()
putULEB128 bh w =
-- #if defined(DEBUG)
    (if w < 0 then panic "putULEB128: Signed number" else id) $
-- #endif
    go w
  where
    go :: a -> IO ()
    go w
      | w <= (127 :: a)
      = putByte bh (fromIntegral w :: Word8)
      | otherwise = do
        -- bit 7 (8th bit) indicates more to come.
        let byte = setBit (fromIntegral w) 7 :: Word8
        putByte bh byte
        go (w `unsafeShiftR` 7)
```

We write out values in little endian order. Starting with the least significant bits.

If we have a value <= 127 we can just write the last byte and are done.

Otherwise we set the 8th bit (which is bit 7) to encode the fact that a reader will have
written an additional byte, shift our value right by 7 bits and repeat.

Reading works similarly.

```haskell
getULEB128 :: forall a. (Integral a, Bits a) => BinHandle -> IO a
getULEB128 bh =
    go 0 0
  where
    go :: Int -> a -> IO a
    go shift w = do
        b <- getByte bh
        let hasMore = testBit b 7
        let !val = w .|. ((clearBit (fromIntegral b) 7) `unsafeShiftL` shift) :: a
        if hasMore
            then do
                go (shift+7) val
            else
                return $! val
```

We read a byte, combine it with what we have read already
and check the mark bit if we need to read more bytes.

The only noteworthy thing here is that we are using a little endian encoding
so we have to shift bits read later farther to the right. Which we can do by
keeping track of the current shift offset - `shift` in the code above.

### Signed values

Signed values are slightly tricker because of sign extension.

```haskell
-- Signed numbers
putSLEB128 :: forall a. (Integral a, Bits a) => BinHandle -> a -> IO ()
putSLEB128 bh initial = go initial
  where
    go :: a -> IO ()
    go val = do
        let byte = fromIntegral (clearBit val 7) :: Word8
        let val' = val `unsafeShiftR` 7
        let signBit = testBit byte 6
        let done =
                -- Unsigned value, val' == 0 and and last value
                -- can be discriminated from a negative number.
                ((val' == 0 && not signBit) ||
                -- Signed value,
                 (val' == -1 && signBit))

        let byte' = if done then byte else setBit byte 7
        putByte bh byte'

        unless done $ go val'
```

We still write out numbers 7 bits a piece, but our termination condition is different.

The reason is how negative numbers are [encoded](https://en.wikipedia.org/wiki/Two%27s_complement).

The main things to keep in mind here are that:  
* Shifting a negative value to the right will never become zero because
of sign extension, so we can't just check for zero. You can convince yourself that this is true with `unsafeShiftR (-1) n` for any n.
* The highest data bit in our encoded data stream will tell the reader the sign of the read value. It will be set for negative numbers and unset otherwise.
* Further there is no negative zero, this means the highest negative number possible is -1 (unlike with floats).

We can stop writing bytes only if a reader can reconstruct both the value and the sign.

This means the highest data bit (`signBit`) written must match the sign of the value,
and there are no more data bits to be written. Which is what we compute for `done`.

Reading again the same in reverse.

```haskell
getSLEB128 :: forall a. (Integral a, FiniteBits a) => BinHandle -> IO a
getSLEB128 bh = do
    (val,shift,signBit) <- go 0 0
    if signBit && (shift < finiteBitSize val )
        -- Manual sign extension
        then return $ ((complement 0 `unsafeShiftL` shift) .|. val)
        else return val
    where
        go :: Int -> a -> IO (a,Int,Bool)
        go shift val = do
            byte <- getByte bh
            let byteVal = fromIntegral (clearBit byte 7) :: a
            let !val' = val .|. (byteVal `unsafeShiftL` shift)
            let more = testBit byte 7
            let shift' = shift+7
            if more
                then go (shift') val'
                else do
                    let !signBit = testBit byte 6
                    return (val',shift',signBit)
```

When we read the last byte we check the `signBit` to reconstruct the sign. If we deal with a negative
value we manually [sign extend](https://en.wikipedia.org/wiki/Sign_extension) the resulting value properly.

## Applying the change to GHC

The change itself then isn't all that much, we just use our functions inside the instances.

```haskell
instance Binary Word16 where
  put_ = putULEB128
  get  = getULEB128

instance Binary Word32 where
  put_ = putULEB128
  get  = getULEB128

...

```

I've tested the encodings heavily outside GHC. Primarily because any change to the Binary instances
requires a full rebuild, to avoid files persisting which still use the old instances.

However even so I only got a failed build.

```
"inplace/bin/ghc-stage1.exe" -optc-Wall -optc-Ilibraries/ghc-prim/dist-install/build/./autogen -optc-Ilibraries/ghc-prim/. -optc-I'E:/ghc_head/rts/dist/build' -optc-I'E:/ghc_head/includes' -optc-I'E:/ghc_head/includes/dist-derivedconstants/header' -optc-Wno-error=inline -optc-Wno-sync-nand -static  -O -H64m -Wall      -this-unit-id ghc-prim-0.6.1 -hide-all-packages -i -ilibraries/ghc-prim/. -ilibraries/ghc-prim/dist-install/build -Ilibraries/ghc-prim/dist-install/build -ilibraries/ghc-prim/dist-install/build/./autogen -Ilibraries/ghc-prim/dist-install/build/./autogen -Ilibraries/ghc-prim/.    -optP-include -optPlibraries/ghc-prim/dist-install/build/./autogen/cabal_macros.h -package-id rts -this-unit-id ghc-prim -XHaskell2010 -O2 -O  -no-user-package-db -rtsopts  -Wno-trustworthy-safe -Wno-deprecated-flags     -Wnoncanonical-monad-instances  -c libraries/ghc-prim/cbits/atomic.c -o libraries/ghc-prim/dist-install/build/cbits/atomic.o
ghc-stage1.exe: panic! (the 'impossible' happened)
  (GHC version 8.9.0.20190815:
        Ix{Int}.index: Index (2291798952) out of range ((0,827))

Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug

make[1]: *** [libraries/ghc-prim/ghc.mk:4: libraries/ghc-prim/dist-install/build/GHC/CString.o] Error 1
make[1]: *** Waiting for unfinished jobs....
ghc-stage1.exe: panic! (the 'impossible' happened)
  (GHC version 8.9.0.20190815:
        Ix{Int}.index: Index (2291798952) out of range ((0,827))

Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug

ghc-stage1.exe: panic! (the 'impossible' happened)
  (GHC version 8.9.0.20190815:
        Ix{Int}.index: Index (2291798952) out of range ((0,827))

Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug

make[1]: *** [libraries/ghc-prim/ghc.mk:4: libraries/ghc-prim/dist-install/build/GHC/IntWord64.o] Error 1
make[1]: *** [libraries/ghc-prim/ghc.mk:4: libraries/ghc-prim/dist-install/build/GHC/Prim/Ext.o] Error 1
make: *** [Makefile:128: all] Error 2
```

`Ix{Int}.index: Index (2291798952) out of range ((0,827))` is a typical error when a Binary instance
does not agree on the number of bytes read/written in get/put.

### Limiting scope

After a lot of recompilations warming up my room I eventually noticed that
using the new encoding works, except when I use it for the Word32 instance.

So I simply removed the instance and let GHC tell me where it was used.


### The culprit

Given that roundtripping worked I suspected early on that some usage of
the instance relies on it's encoded size. And indeed while tracking down
instance usages eventually I found this use case:

```haskell
putWithUserData :: Binary a => (SDoc -> IO ()) -> BinHandle -> a -> IO ()
putWithUserData log_action bh payload = do
    -- Remember where the dictionary pointer will go
    dict_p_p <- tellBin bh
    -- Placeholder for ptr to dictionary
    put_ bh dict_p_p

    -- Write a lot of data
    -- ...
    -- And then ...

    -- Write the dictionary pointer at the front of the file
    dict_p <- tellBin bh          -- This is where the dictionary will start
    putAt bh dict_p_p dict_p      -- Fill in the placeholder
    seekBin bh dict_p             -- Seek back to the end of the file
```

Here `dict_p_p` was using the Word32 instance.

The problem is that the placeholder might end up being **smaller** than the actual value.
So writing into the reserved spot might (and did) overwrite data following it.

For example we might get:

```haskell
    -- Get position in stream, let's say it's zero.
    dict_p_p <- tellBin bh
    
    -- Write position as single byte 0
    put_ bh dict_p_p
    
    -- Write a lot of data
    -- ...
    -- And then ...

    -- Position here might be (in variable length encoding) [0xFF, 0x01]
    dict_p <- tellBin bh
    -- Will write two bytes, but only one byte was reserved above.
    putAt bh dict_p_p dict_p
```

So I simple changed the `Bin` (stream position) instance to write a fixed number of bytes and things worked out.

## Results

The end result of this is that we save about 18% in interface file size.

Before:  
```
Andi@Horzube MINGW64 /e/ghc_head
$ ls nofib/spectral/simple/Main.hi -l
-rw-r--r-- 1 Andi None 116501 Aug 14 23:35 nofib/spectral/simple/Main.hi
```

After:  
```
$ ls nofib/spectral/simple/Main.hi -l
-rw-r--r-- 1 Andi None 96058 Aug 15 19:42 nofib/spectral/simple/Main.hi
```

Although the actual benefit varies depending on the file in question.
I've seen some with >25% size reductions and others closer to 10%. But
it's a decent win in all cases.
