---
title: 64 Bit Words might be bigger than you think.
author: Andreas Klebinger
categories: Trivia
---

Every once in a while I've witnessed someone worrying about a counter overflowing.
While those bugs do certainly happen. With things that are counted one at a time
on hardware this is basically impossible.

# Thesis: If you increment one at a time, on hardware, a Word64 is basically infinite.

From an engineering point of view if the hardware is guaranteed to die or your program
to terminate before you exceed 2^64 you can treat a 64bit Words as infinite. Which is
terribly convenient.

It's hard to grasp how much leeway that actually gives us. So here are some examples.

# How long can you count?

Incrementing in steps of one. How long can you count using a 64bit Word?

## Cars

You write a program counting cars passing by. You expect an average of one car
per second to go by. You will be able to count cars for ~40x the age of the universe.
Do not worry about it.

## CPU Cycles

A average high end CPU runs at ~4GHz. If we count the number of cycles one at a
time we can count cycles for ~150 years. Don't worry about it.

## Water drops in the Danube

The danube has a throughput of ~6450m³/s. That is about 0.13 Trillion drops of
water per second. If we count each drop one at a time we can count water drops
for 43 Months. You probably don't need to worry about it.

## Photons hitting earth.

It seems ~10^20 Photons hit every m² of earth every seconding. If you count them
one at a time then ... wait what hardware are you running this one. Are you running
our simulation. Why is my screen turning into a matrix screen saver.

Jokes aside there is no hardware that could count photons hitting earth one at a
time. So don't worry about it.

# Word64 is *not* infinite

Things escalate quickly if you aggregate counters. While we can count cycles
for longer than almost all hardware will last. Counting instructions on a Server
might run into roll over territory. E.g. 192 cores * 2.5GHz * 3 (IPC)
would give you a lower bound of 148 days before roll over.

But if you are counting things one at a time, on hardware, without adding up counters?
Don't worry about it.
