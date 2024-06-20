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
        assert_eq!(result3, 2);

        let result4 = fibonacci(4);
        assert_eq!(result4, 3);

        let result5 = fibonacci(5);
        assert_eq!(result5, 5);

        let result6 = fibonacci(6);
        assert_eq!(result6, 8);
    }
    
}
mod fib;

fn fibonacci(n: u64) -> u64 {
    match n {
        0 => 1,
        1 => 1,
        _ => fibonacci(n-1) + fibonacci(n-2),
    }
}
