
# Table of Contents

1.  [About](#orgefea846)
2.  [Why?](#orgf08e16c)
3.  [License](#org3388864)
4.  [Design Ideas](#org8f0c3d5)
    1.  [MVP](#orgb6746ff)
    2.  [More Complex Examples](#org9eb4ff3)
    3.  [stdlib](#org2ceaab5)
5.  [Resources](#org9e35b18)
6.  [Things To Research](#org9d66d2f)
    1.  [Rust calling convention](#org5478e77)
    2.  [Basic Data Type Conversion](#org83fed55)
        1.  [How about the stdlib?](#orgf3d6cb8)
    3.  [Rust Metadata](#orgaf470be)
        1.  [How to programmatically inspect crates](#orgb78bc8b)
        2.  [How to find public API of a crate?](#org90b9e25)
    4.  [Rust generics](#org0e3f68f)
        1.  [How do they work?](#org1277132)
        2.  [Can they work with Common Lisp objects?](#org2007298)
    5.  [Callbacks](#org7959cc7)
    6.  [TODO:](#orgae9514e)


<a id="orgefea846"></a>

# About

This is a package to call Rust library crates from Common Lisp.

Right now it's in a very early stage, and has no working code.

I'm currently flushing out the API design, researching Rust and its implementation, and
studying the CFFI to understand how this may work.


<a id="orgf08e16c"></a>

# Why?

If Rust is going to replace C++ and C, it must be possible to use Rust from 3rd party languages,
like Common Lisp and Python.  I think Common Lisp has enough flexibility to make working with Rust
seamless.


<a id="org3388864"></a>

# License

ISC

Copyright (c) 2024 Jeremiah LaRocco <jeremiah<sub>larocco</sub>@fastmail.com>


<a id="org8f0c3d5"></a>

# Design Ideas

The long term goal is to make using Rust from CL as transparent as possible.


<a id="orgb6746ff"></a>

## MVP

    (ql:quickload '(:rffi))
    
    ;; Load the crate
    (rffi:use-crate "simple-lib")
    
    ;; Call a function and get a result.
    (format t "~a~%" (simple-lib:fibonacci 10))


<a id="org9eb4ff3"></a>

## More Complex Examples

Eventually more advanced crates like rstar::RTree should also work.

<https://docs.rs/rstar/latest/rstar/struct.RTree.html>

Note the integration with iterate.

    (ql:quickload '(:rffi :iterate))
    (use-package :iterate)
    
    (rffi:use-crate "rstar::RTree")
    
    ;; Create an RTree
    (let ((tree (rtree:rtree-new)))
    
      ;; Insert
      (rtree:insert tree '(0.1f0 0.0f0))
      (rtree:insert tree '(0.2f0 0.1f0))
      (rtree:insert tree '(0.3f0 0.1f0))
      (assert (equal (rtree:nearest-neighbor tree '(0.4f0 -0.1f0)) '(0.3f0 0.0f0)))
    
      ;; Remove
      (rtree:remove tree '(0.4f0 0.0f0))
      (assert (equal (rtree:nearest-neighbor tree '(0.4f0 0.3f0)) '(0.2f0 0.1f0)))
      (assert (= (rtree:size tree) 2))
    
      ;; Iterate support for Rust iterators
      (iterate
        (for elt :in tree)
        (format t "~a~%" elt)))


<a id="org2ceaab5"></a>

## stdlib

The stdlib should also be usable, but AFAICT, it's not a crate.

In any case, if it behaved like a crate for the sake of the binding, that would be great.

In that case, HashMap might look something like this
<https://doc.rust-lang.org/std/collections/struct.HashMap.html>

    (ql:quickload '(:rffi :iterate))
    (use-package :iterate)
    
    (rffi:use-crate "std::collections::HashMap")
    
    ;; Create a hashmap
    (let ((book-reviews (hash-map:new)))
    
      ;; Insert
      (hash-map:insert book-reviews "Adventures of Huck Finn" "My favorite!")
      (hash-map:insert book-reviews "The C++ Programming language" "Bjarne!")
      (hash-map:insert book-reviews "Lisp" "Henry Winston")
    
      ;; Query
      (when (not (hash-map:contains-key book-reviews "Let Over Lambda"))
        (format t "Did not find LOL in reviews.~%"))
    
      ;; Remove
      (hash-map:remove book-reviews "Adventures of Huck Finn")
    
      ;; Fetch
      (loop :for book :in '("Pride and Prejudice", "Lisp")
            ;; None -> nil
            :for review = (hash-map:get book-reivews book)
            :when review
                  (format t "Review for ~a is: ~s~%" book review))
    
      ;; Iterate
      (iterate
        (for (book . review) :in book-reviews
             (format "~s : ~s~%" book review)))) 


<a id="org9e35b18"></a>

# Resources

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">ABI Info</td>
<td class="org-left"><a href="https://www.youtube.com/watch?v=MY5kYqWeV1Q">https://www.youtube.com/watch?v=MY5kYqWeV1Q</a></td>
</tr>

<tr>
<td class="org-left">A crate implementing a stable ABI</td>
<td class="org-left"><a href="https://docs.rs/abi_stable/latest/abi_stable/">https://docs.rs/abi_stable/latest/abi_stable/</a></td>
</tr>

<tr>
<td class="org-left">Another crate for a stable ABI</td>
<td class="org-left"><a href="https://github.com/ZettaScaleLabs/stabby">https://github.com/ZettaScaleLabs/stabby</a></td>
</tr>

<tr>
<td class="org-left">RFFI must support APIs following these</td>
<td class="org-left"><a href="https://rust-lang.github.io/api-guidelines/about.html">https://rust-lang.github.io/api-guidelines/about.html</a></td>
</tr>

<tr>
<td class="org-left">Info about collections</td>
<td class="org-left"><a href="https://github.com/rust-lang/compiler-builtins">https://github.com/rust-lang/compiler-builtins</a></td>
</tr>

<tr>
<td class="org-left">Minimizing size</td>
<td class="org-left"><a href="https://github.com/johnthagen/min-sized-rust#optimize-libstd-with-build-std">https://github.com/johnthagen/min-sized-rust#optimize-libstd-with-build-std</a></td>
</tr>

<tr>
<td class="org-left">Rust + libc (why not vDSO or syscall ?)</td>
<td class="org-left"><a href="https://users.rust-lang.org/t/clarifications-on-rusts-relationship-to-libc/56767">https://users.rust-lang.org/t/clarifications-on-rusts-relationship-to-libc/56767</a></td>
</tr>

<tr>
<td class="org-left">Just interesting</td>
<td class="org-left"><a href="https://stackoverflow.com/questions/12806584/what-is-better-int-0x80-or-syscall-in-32-bit-code-on-linux">https://stackoverflow.com/questions/12806584/what-is-better-int-0x80-or-syscall-in-32-bit-code-on-linux</a></td>
</tr>

<tr>
<td class="org-left">Just interesting</td>
<td class="org-left"><a href="https://github.com/rust-lang/rust/blob/master/library/std/src/collections/mod.rs">https://github.com/rust-lang/rust/blob/master/library/std/src/collections/mod.rs</a></td>
</tr>

<tr>
<td class="org-left">Rust "Core" library details</td>
<td class="org-left"><a href="https://doc.rust-lang.org/core/">https://doc.rust-lang.org/core/</a></td>
</tr>

<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>


<a id="org9d66d2f"></a>

# Things To Research


<a id="org5478e77"></a>

## Rust calling convention


<a id="org83fed55"></a>

## Basic Data Type Conversion

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">f32</td>
<td class="org-left">single-float</td>
</tr>

<tr>
<td class="org-left">f64</td>
<td class="org-left">double-float</td>
</tr>

<tr>
<td class="org-left">i64</td>
<td class="org-left">fixnum or (signed-byte 64)</td>
</tr>

<tr>
<td class="org-left">u64</td>
<td class="org-left">integer or (usigned-byte 64)</td>
</tr>

<tr>
<td class="org-left">i32</td>
<td class="org-left">fixnum or (signed-byte 32)</td>
</tr>

<tr>
<td class="org-left">u32</td>
<td class="org-left">fixnum or (signed-byte 32)</td>
</tr>

<tr>
<td class="org-left">string</td>
<td class="org-left">string</td>
</tr>

<tr>
<td class="org-left">array</td>
<td class="org-left">array (or list?)</td>
</tr>
</tbody>
</table>


<a id="orgf3d6cb8"></a>

### How about the stdlib?

These conversions should be possible and easy to do, but I don't think it makes sense to do them automatically.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">Vec</td>
<td class="org-left">array</td>
</tr>

<tr>
<td class="org-left">hashmap</td>
<td class="org-left">hashtable</td>
</tr>
</tbody>
</table>


<a id="orgaf470be"></a>

## Rust Metadata


<a id="orgb78bc8b"></a>

### How to programmatically inspect crates

    cargo metadata [<crate name>]


<a id="org90b9e25"></a>

### How to find public API of a crate?

Read .rlib file somehow? 


<a id="org0e3f68f"></a>

## Rust generics


<a id="org1277132"></a>

### How do they work?

Are they like C++ templates - effectively compile-time code generation?


<a id="org2007298"></a>

### Can they work with Common Lisp objects?

Can there be a Rust wrapper around CL objects?


<a id="org7959cc7"></a>

## Callbacks

Is it possible to pass Lisp functions into Rust?
Is it possible to pass Rust functions into Lisp?


<a id="orgae9514e"></a>

## TODO:

