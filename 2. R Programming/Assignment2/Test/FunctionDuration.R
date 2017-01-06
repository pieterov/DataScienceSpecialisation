FunctionDuration <- function(fun,...) {
        
        start_time <- proc.time()
        
        result <- fun(...)
        
        list(proc.time() - start_time, result)
        
        }