
# Table of Contents

1.  [About](#org7ed904a)
2.  [License](#orgb655e87)
3.  [Design Ideas](#orgfe70b78)
    1.  [MVP](#org8bcea49)
    2.  [More Complex Examples](#orgcc0caf9)
    3.  [stdlib](#org656d2ce)
4.  [Resources](#org94bc9c7)
5.  [Things To Research](#org877be74)
    1.  [Rust calling convention](#org476921f)
    2.  [Basic Data Type Conversion](#orgab157be)
        1.  [How about the stdlib?](#org7dc69f4)
    3.  [Rust Metadata](#orgd717456)
        1.  [How to programmatically inspect crates](#orgdd78aa7)
        2.  [How to find public API of a crate?](#orgfc95ffe)
    4.  [Rust generics](#org0c6df03)
        1.  [How do they work?](#org2c71446)
        2.  [Can they work with Common Lisp objects?](#orgb6fb03d)
    5.  [Callbacks](#orgc85b914)
    6.  [TODO:](#orgc85fd6d)


<a id="org7ed904a"></a>

# About

This is a package to call Rust library crates from Common Lisp.

Right now it's in a very early stage, and has no working code.

I'm currently flushing out the API design, researching Rust and its implementation, and
studying the CFFI to understand how this may work.


<a id="orgb655e87"></a>

# License

ISC

Copyright (c) 2024 Jeremiah LaRocco <jeremiah<sub>larocco</sub>@fastmail.com>


<a id="orgfe70b78"></a>

# Design Ideas

The long term goal is to make using Rust from CL as transparent as possible.


<a id="org8bcea49"></a>

## MVP

    (ql:quickload '(:rffi))
    
    ;; Load the crate
    (rffi:use-crate "simple-lib")
    
    ;; Call a function and get a result.
    (format t "~a~%" (simple-lib:fibonacci 10))


<a id="orgcc0caf9"></a>

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


<a id="org656d2ce"></a>

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


<a id="org94bc9c7"></a>

# Resources

<https://www.youtube.com/watch?v=MY5kYqWeV1Q>
<https://docs.rs/abi_stable/latest/abi_stable/>
<https://github.com/ZettaScaleLabs/stabby>
<https://rust-lang.github.io/api-guidelines/about.html>
<https://github.com/rust-lang/compiler-builtins>
<https://github.com/rust-lang/rust/blob/master/library/std/src/collections/mod.rs>
<https://github.com/johnthagen/min-sized-rust#optimize-libstd-with-build-std>


<a id="org877be74"></a>

# Things To Research


<a id="org476921f"></a>

## Rust calling convention


<a id="orgab157be"></a>

## Basic Data Type Conversion

f32 -> single-float
f64 -> double-float
i64 -> fixnum or (signed-byte 64)
u64 -> integer or (usigned-byte 64)
i32 -> fixnum or (signed-byte 32)
u32 -> fixnum or (signed-byte 32)
string -> string
array -> array (or list?)


<a id="org7dc69f4"></a>

### How about the stdlib?

These conversions should be possible and easy to do, but I don't think it makes sense to do them automatically.

Vec -> array
hashmap -> hashtable


<a id="orgd717456"></a>

## Rust Metadata


<a id="orgdd78aa7"></a>

### How to programmatically inspect crates

    cargo metadata [<crate name>]


<a id="orgfc95ffe"></a>

### How to find public API of a crate?

Read .rlib file somehow? 


<a id="org0c6df03"></a>

## Rust generics


<a id="org2c71446"></a>

### How do they work?

Are they like C++ templates - effectively compile-time code generation?


<a id="orgb6fb03d"></a>

### Can they work with Common Lisp objects?

Can there be a Rust wrapper around CL objects?


<a id="orgc85b914"></a>

## Callbacks

Is it possible to pass Lisp functions into Rust?
Is it possible to pass Rust functions into Lisp?


<a id="orgc85fd6d"></a>

## TODO:

