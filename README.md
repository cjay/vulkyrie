# Vulkyrie
A graphics engine in Haskell based on Vulkan, using 
[vulkan-api](https://github.com/achirkin/vulkan), a low-level low-overhead binding to Vulkan.
Initially forked from 
[the example project](https://github.com/achirkin/vulkan/tree/master/vulkan-triangles) that is based on vulkan-tutorial.com.

This is still in an early alpha stage, but should already be a useful starting point for people looking how to do more than 
just draw a triangle with Vulkan in Haskell.

For a non-trivial example project see [some-roguelike](https://github.com/cjay/some-roguelike).

If you want to try, as of right now you should use GHC 8.8, as I'm waiting for multiple dependencies to accept my pull requests for 8.10.
