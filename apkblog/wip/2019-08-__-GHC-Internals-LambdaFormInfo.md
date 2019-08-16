---
title: GHC Internals - Improving pointer tagging across module boundaries.
tags: Haskell, GHC, vector, Assembly, optimization
---

GHC uses pointer tagging to encode in an objects pointer information about the object.

However this was limited when a pointer references an object from another module.

This post goes over how we lifted this restriction.

# GHC's information about variable references: LambdaFormInformation

GHC uses LambdaFormInfo to encode information about how it should treat object references
during compilation.

```haskell
data LambdaFormInfo
  = LFReEntrant         -- Reentrant closure (a function)
        TopLevelFlag    -- True if top level
        OneShotInfo
        !RepArity       -- Arity. Invariant: always > 0
        !Bool           -- True <=> no fvs
        ArgDescr        -- Argument descriptor (should really be in ClosureInfo)

  | LFThunk             -- Thunk (zero arity)
        TopLevelFlag
        !Bool           -- True <=> no free vars
        !Bool           -- True <=> updatable (i.e., *not* single-entry)
        StandardFormInfo
        !Bool           -- True <=> *might* be a function type

  | LFCon               -- A saturated constructor application
        DataCon         -- The constructor

  | LFUnknown           -- Used for function arguments and imported things.
                        -- We know nothing about this closure.
                        -- Treat like updatable "LFThunk"...
                        -- Imported things which we *do* know something about use
                        -- one of the other LF constructors (eg LFReEntrant for
                        -- known functions)
        !Bool           -- True <=> *might* be a function type
                        --      The False case is good when we want to enter it,
                        --        because then we know the entry code will do
                        --        For a function, the entry code is the fast entry point

  | LFUnlifted          -- A value of unboxed type;
                        -- always a value, needs evaluation

  | LFLetNoEscape       -- See LetNoEscape module for precise description
```

# The problem

The problem here is LFUnknown. It forces us to assume safe defaults, which is always correct but often worse code.

For example we might define a value in one Module, but use it in a different one:

```haskell
module A where
...
{-# NOINLINE value #-}
value :: Maybe Integer
value = Just 1
```

```haskell
module Main where

main = ...
	case value of
		...
```

Here value is already fully evaluated at compile time.
Nevertheless we end up with code which will:
* Check if value is tagged
* Determine it is not tagged
* Jump into value's closure
* Return a tagged reference to value.
* And only then continue in a fashion similar to what we get when value is defined in the same module.

This is not terribly common, but still common. But let's fix this!

# In order to fix this we need to:

* Export the LFInfo (LambdaFormInfo) when compiling a module.
* Read the exported info when compiling a module with imports.
* Use the imported info for code generation.

We will do this one step at a time.

## Exporting the information.

We start by looking into the code which generates the information.

I happen to know that we produce this information during code generation.
Code generation (Stg to Cmm at least) is done by this function.

```haskell
codeGen :: DynFlags
        -> Module
        -> [TyCon]
        -> CollectedCCs                -- (Local/global) cost-centres needing declaring/registering.
        -> [CgStgTopBinding]           -- Bindings to convert
        -> HpcInfo
        -> Stream IO CmmGroup ()       -- Output as a stream, so codegen can
                                       -- be interleaved with output
```

Since the LFInfo is generated during codegen we must also return it:

```haskell
codeGen :: ...
        -> Stream IO CmmGroup [(Id,LambdaFormInformation)]
                              
```

We can then chase down type errors. I won't go into the details but it boiled down
to update the type of streams used to carry along the stream result.

It also required changing a few places in which we did so far throw away the result of
the stream. e.g.:

```haskell
module Stream where
...

-- Was mapAccumL in the past.
mapAccumL_ :: Monad m => (c -> a -> m (c,b)) -> c -> Stream m a ()
           -> Stream m b c
...

-- New version
mapAccumL :: Monad m => (c -> a -> m (c,b)) -> c -> Stream m a d
          -> Stream m b (c,d)
          
```

But this was all very straight forward.

### Where the data comes out of the stream.

Eventually inside `HscMain:hscGenHardCode` we run the stream and collect the result.

I defined a Outputable instance so we can pretty print the resulting LFInfos:

```
instance Outputable LambdaFormInfo where
    ppr (LFReEntrant top oneshot rep fvs argdesc) =
        text "LFReEntrant" <> brackets (ppr top <+> ppr oneshot <+>
                                        ppr rep <+> ppr fvs <+> ppr argdesc)
    ppr (LFThunk top hasfv updateable sfi m_function) =
        text "LFThunk" <> brackets (ppr top <+> ppr hasfv <+> ppr updateable <+>
                                    ppr sfi <+> ppr m_function)
    ppr (LFCon con) = text "LFCon" <> brackets (ppr con)
    ppr (LFUnknown m_func) =
        text "LFUnknown" <>
            if m_func
                then brackets (text "mf")
                else empty
    ppr (LFUnlifted) = text "LFUnlifted"
    ppr (LFLetNoEscape) = text "LF-LNE"
```

Now we can check if this worked by using `pprTraceM "CgInfo" $ ppr stream_result` inside
hscGenHardCode. Compile ghc, and then compile something:

```
$ inplace/bin/ghc-stage2.exe nofib/spectral/dom-lt/Dom.hs -O -fforce-recomp -c
CgInfo
  [(ancestors, LFReEntrant[<TopLevel> 1 True ArgSpec 5]),
   (asGraph, LFReEntrant[<TopLevel> 1 True ArgSpec 5]),
   (asTree, LFReEntrant[<TopLevel> 1 True ArgSpec 5]),
   ...
   (pdomTree, LFReEntrant[<TopLevel> 1 True ArgSpec 5]),
   (rpddfs, LFReEntrant[<TopLevel> 1 True ArgSpec 5]),
   (toAdj, LFReEntrant[<TopLevel> 1 True ArgSpec 5]),
   (toEdges, LFReEntrant[<TopLevel> 1 True ArgSpec 5]),
   ($fApplicativeS, LFCon[C:Applicative]),
   ($fFunctorS, LFCon[C:Functor]),
   ($fApplicativeS5, LFReEntrant[<TopLevel> 2 True ArgSpec 15]),
   ($fApplicativeS4, LFReEntrant[<TopLevel> 3 True ArgSpec 23]),
   ...
  ] 

```


## Interface file generation

As a next step we need to figure out when/how interface files are written.
This is mostly documenting what I found, not how I got there.

All of this happens in HscMain.hs

* `hscMaybeWriteIface` is the function actually used to write interface files.
* Which is called by finish
* Which is called by `hscIncrementalCompile`
* Which is called from `DriverPipeline.hs`

But what we want is `hscMaybeWriteIface` to be called in `hscGenHardCode`. However `hscGenHardCode` is called from `DriverPipeline.hs`.

So what we likely want to do is refactor this logic such that:
* `hscGenHardCode` returns whatever we want to put into the interface file.
* At it's call site we put this into the interface file and write it out.

We can modify interfaces, but we have to be careful to only add fingerprints once all is said and done.

## State of things in the compiler driver:

* `finish` *creates* AND writes the interface file.
* `hscIncrementalCompile` calls finish.

There are two main codepaths eventually calling `hscIncrementalCompile`.

### Via `runPipeline`:

* `runPipeline` calls `pipeLoop`.
* Which calls `runHookedPhase`, runs hooks and ...
* Which calls `runPhase` (Hsc)
      + Which calls **`hscIncrementalCompile`**
      + And eventually calls ...
* `runPhase` with `HscOut HscSource ModuleName HscStatus`
      + Which calls **`hscGenHardCode`**

### Alternative code path: `compileOne'`

* Starts at `compileOne'`
      + Calls `hscIncrementalCompile`
      + In case of status == `HscRecomp`:
      + Calls `runPipeline`
      + returns the generated iface inside `ModHome` generated by `hasIncrementalCompile`.

## Pushing back interface file generation.

We have two goals:
* Don't change the behaviour for any case that doesn't run the codeGen.
* Change the behaviour when we run codeGen to write the iface *after* codeGen has run.

As it turns out `HscStatus` is part of what we get back from `hscIncrementalCompile`.
It indicates if we need to run codeGen after having processed the core.

```haskell
-- | Status of a compilation to hard-code
data HscStatus
    = HscNotGeneratingCode
    | HscUpToDate
    | HscUpdateBoot
    | HscUpdateSig
    | HscRecomp CgGuts ModSummary
```

We only run the code generator if we get back the status HscRecomp!  
So we simply expand it with the info required to emit interface files later:

```haskell
-- | Status of a compilation to hard-code
data HscStatus
    = HscNotGeneratingCode  -- ^ Nothing to do.
    | HscUpToDate           -- ^ Nothing to do because code already exists.
    | HscUpdateBoot         -- ^ Update boot file result.
    | HscUpdateSig          -- ^ Generate signature file (backpack)
    | HscRecomp             -- ^ Recompile this module.
        { hscs_guts       :: CgGuts -- ^ Information for the code generator.
        , hscs_summary    :: ModSummary -- ^ Module info
        -- TODO: Should we drop the change flag and just always write it?
        , hscs_iface_info :: (ModIface, IfaceChanged)
                            -- ^ Info required to update iface.

        }

-- | Only when we run the stg+ parts of the compiler
-- we have to update the iface after codegen.
needsIfaceUpdate :: HscStatus -> Bool
needsIfaceUpdate HscRecomp {} = True
needsIfaceUpdate _            = False
```

Now inside finish where we originally *always* wrote out the interface file instead we can use:

```haskell
  ... -- do things
  unless (needsIfaceUpdate hsc_status) $ do
    liftIO $ hscMaybeWriteIface dflags iface no_change (ms_location summary)
  return
    ( hsc_status
    , HomeModInfo
      {hm_details = details, hm_iface = iface, hm_linkable = Nothing})
```

This means interface files are always generated after core compilation UNLESS we run further
codeGen passes.

### Late interface file generation

Now `hscIncrementalCompile` is generally called in a fashion like this:

```haskell      
      (result, _ ) <- liftIO $ hscIncrementalCompile True Nothing (Just msg) hsc_env'
                            mod_summary source_unchanged Nothing (1,1)

      -- Return next pass/phase to run.
      return (HscOut src_flavour mod_name result,
                panic "HscOut doesn't have an input filename")
```

The result of which is used in `runPhase`:

```haskell
runPhase (HscOut src_flavour mod_name result ) _ dflags = do
        location <- getLocation src_flavour mod_name
        setModLocation location

        let o_file = ml_obj_file location -- The real object file
            hsc_lang = hscTarget dflags
            next_phase = hscPostBackendPhase src_flavour hsc_lang

        case result of
            HscNotGeneratingCode ->
                -- we do nothing
            HscUpToDate ->
                do -- touches .o file
            HscUpdateBoot ->
                do -- touches .o file
            HscUpdateSig ->
                do -- creates a empty .o file
            HscRecomp cgguts mod_summary (iface, iface_changed)
              -> do output_fn <- phaseOutputFilename next_phase

                    PipeState{hsc_env=hsc_env'} <- getPipeState

                    (outputFilename, mStub, foreign_files, codeGenInfo) <- liftIO $
                      hscGenHardCode hsc_env' cgguts mod_summary output_fn

                    --------------------------------------------------------------------------
                    --      This part is new  ------------------------------------------------
                    --------------------------------------------------------------------------
                    let iface' = addIfaceCodeGenInfo iface codeGenInfo

                    liftIO $ hscMaybeWriteIface dflags iface' (not $ hasChanged iface_changed)
                                                    (ms_location mod_summary)
                    --------------------------------------------------------------------------


                    stub_o <- liftIO (mapM (compileStub hsc_env') mStub)
                    foreign_os <- liftIO $
                      mapM (uncurry (compileForeign hsc_env')) foreign_files
                    setForeignOs (maybe [] return stub_o ++ foreign_os)

                    return (RealPhase next_phase, outputFilename)
                  where
                    -- -- | Add information produced by codeGen to the iface.
                    -- -- mkIfaceCodegenInfo :: ModIface -> [(Name,LambdaFormInfo)] -> ModIface
                    -- mkIfaceCodegenInfo core_iface lf_info =
                    --   core_iface { mi_lf_info = Just lf_info }
```

Which seems like a decent solution!

## Dealing with DataCon

One of the LambdaFormInfo constructors, `LFCon`, takes as argument a DataCon.
This is nothing special, however we want to avoid serializing the full data con
into the interface file.

Instead we want to either:
* Serialize only the information we need.
* Serialize it by name and look it up afterwards.

The later seems better, but is rather complicated as it turns out as there
is an assumption of being in a form of typechecker Monad.

We start by looking at `tcIfaceGlobal`

```haskell
tcIfaceGlobal :: Name -> IfL TyThing
tcIfaceGlobal name
  | Just thing <- wiredInNameTyThing_maybe name
        -- Wired-in things include TyCons, DataCons, and Ids
        -- Even though we are in an interface file, we want to make
        -- sure the instances and RULES of this thing (particularly TyCon) are loaded
        -- Imagine: f :: Double -> Double
  = do { ifCheckWiredInThing thing; return thing }

  | otherwise
  = do  { env <- getGblEnv :: TcRnIf gbl lcl gbl
        ; case if_rec_types env of {    -- Note [Tying the knot]
            Just (mod, get_type_env)
                | nameIsLocalOrFrom mod name
                -> do           -- It's defined in the module being compiled
                { type_env <- setLclEnv () get_type_env         -- yuk
                ; case lookupNameEnv type_env name of
                    Just thing -> return thing
                    -- See Note [Knot-tying fallback on boot]
                    Nothing   -> via_external
                }

          ; _ -> via_external }}
  where
    via_external =  do
        { hsc_env <- getTopEnv
        ; mb_thing <- liftIO (lookupTypeHscEnv hsc_env name)
        ; case mb_thing of {
            Just thing -> return thing ;
            Nothing    -> do

        { mb_thing <- importDecl name   -- It's imported; go get it
        ; case mb_thing of
            Failed err      -> failIfM err
            Succeeded thing -> return thing
        }}}
```

We know whatever reference we get will have been seen by the core pass already.
This means we can skip some of these steps.

But one step after another, let's start by looking at what the `IfL` Monad carries around.

It's just a type synonym `type IfL = IfM IfLclEnv`  
for a type synonym `type IfM lcl = TcRnIf IfGblEnv lcl`  
for a type synonym `type TcRnIf a b = IOEnv (Env a b)`
for a data type `data IOEnv env a`!

So really what we have is `IOEnv TcGblEnv IfLclEnv`

We do (currently not have a TcGlbEnv available during codeGen)
Important here is `lookupTypeHscEnv`:  
      Find the TyThing for the given Name by using all the resources at our disposal



## Properly encoding LFInfo into interface files

The main problem here is that LFInfo contains a reference to a data con.
So we can not read it directly via the Binary instance but have to store
it's name, and translate that back to the constructor during loading.

We can however do this by loading the constructor when loading the interface file
using `tcIfaceDataCon`.

Only that this seems to cause a loop ..

* tcIfaceDataCon calls
* tcIfaceGlobal:via_external
* lookupTypeHscEnv
* importDecl
* loadInterface
* tcLFInfo
* tcIfaceDataCon
* ....

We can break this cycle via the use of forkM. This delays the computation until we demand
the result (codeGen). At which point the DataCon will be in the cache.


## dynamic-too

This works now on windows. However testing on linux we get this error:

```
libraries/base/GHC/Exception/Type.hs-boot:12:1: error:
    Bad interface file: /home/andreask/ghc_exportTags/libraries/ghc-prim/dist-install/build/GHC/Types.hi
        mismatched interface file ways (wanted "", got "dyn")
   |
12 | import GHC.Types ()
   | ^^^^^^^^^^^^^^^^^^^
```

The problem is that we run the codeGen pipeline.

In pipeLoop we have:

```haskell
   _
     -> do liftIO $ debugTraceMsg dflags 4
                                  (text "Running phase" <+> ppr phase)
           (next_phase, output_fn) <- runHookedPhase phase input_fn dflags
           r <- pipeLoop next_phase output_fn
           case phase of
               HscOut {} ->
                   whenGeneratingDynamicToo dflags $ do
                       setDynFlags $ dynamicTooMkDynamicDynFlags dflags
                       -- TODO shouldn't ignore result:
                       _ <- pipeLoop phase input_fn
                       return ()
               _ ->
                   return ()
           return r
```

The effect of which is we run the pipeline twice, once with "regular" dynflags, and once with them modified by `dynamicTooMkDynamicDynFlags`.

However this also **unsets* Opt_DynamicToo for the second run, which confuses `hscWriteIface`
which writes out the interface file.

```haskell
hscWriteIface :: DynFlags -> ModIface -> Bool -> ModLocation -> IO ()
hscWriteIface dflags iface no_change mod_location = do
    pprTraceM "Suffixes:" $ ppr (dynOutputFile dflags,
                        dynHiSuf dflags,
                        dynObjectSuf dflags,
                        hiSuf dflags)

    let ifaceFile = ml_hi_file mod_location
    unless no_change $
        {-# SCC "writeIface" #-}
        writeIfaceFile dflags ifaceFile iface
    whenGeneratingDynamicToo dflags $ do
        -- TODO: We should do a no_change check for the dynamic
        --       interface file too
        -- TODO: Should handle the dynamic hi filename properly
        let dynIfaceFile = replaceExtension ifaceFile (dynHiSuf dflags)
            dynIfaceFile' = addBootSuffix_maybe (mi_boot iface) dynIfaceFile
            dynDflags = dynamicTooMkDynamicDynFlags dflags
        writeIfaceFile dynDflags dynIfaceFile' iface
```

HscRecomp
## Binary instances

## Reading the interface files

## Loading the interface file

This concludes the obvious work required.
