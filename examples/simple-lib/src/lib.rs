#[cfg(test)]
mod tests {
    mod fib;
    use crate::fib::fibonacci;
    
    #[test]
    fn it_works() {
        let result = fibonacci(0);
        assert_eq!(result, 1);

        let result1 = fibonacci(1);
        assert_eq!(result1, 1);

        let result2 = fibonacci(2);
        assert_eq!(result2, 2);

        let result3 = fibonacci(3);
        assert_eq!(result3, 3);

        let result4 = fibonacci(4);
        assert_eq!(result4, 5);

        let result5 = fibonacci(5);
        assert_eq!(result5, 8);

        let result6 = fibonacci(6);
        assert_eq!(result6, 13);
    }
    
}

mod fib;


// (defun fibonacci (n)
//   (if (or (= n 0) (= n 1))
//      1
//      (+ (fibonacci (- n 1))
//         (fibonacci (- n 2)))))

fn fibonacci(n: u64) -> u64 {
    match n {
        0 => 1,
        1 => 1,
        _ => fibonacci(n-1) + fibonacci(n-2),
    }
}
