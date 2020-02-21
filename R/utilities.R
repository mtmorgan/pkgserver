globalVariables(".")

.message <-
    function(...)
{
    message(...)
    TRUE
}

.warning <-
    function(...)
{
    warning(..., call. = FALSE)
    TRUE
}
