### Utility routines.
## Written by Danny McClanahan, Irish Lab June 2017.
## <danieldmcclanahan@gmail.com>



library(magrittr)



### Type checking and assertions.
closure_pure <- function (block) {
    bs <- substitute(block)
    eval.parent(bs)
}

int__convert_named_arglist <- function (x) {
    n <- length(x)
    if (0 == n) { return(list()) }
    alt <- as.list(x)
    nms <- names(alt)
    name_only <- (nms == '')
    objs <- rep(alist(. =), n)
    objs[!name_only] <- alt[!name_only]
    nms[name_only] <- alt[name_only]
    setNames(objs, unlist(nms))
}

int__conf_list <- function (...) {
    args <- as.list(substitute(list(...)))[-1L]
    env <- parent.frame()
    f <- bquote(function (arg) { eval(arg, .(env)) })
    int__convert_named_arglist(args) %>% lapply(f)
}

int__merge_env_strats <- list(
    left = setdiff,
    right = setdiff,
    both = intersect,
    either = union,
    neither = function (x, y) {
        ld <- setdiff(x, y)
        rd <- setdiff(y, x)
        intersect(ld, rd)
    })

merge.env <- function (base, new, strategy = int__merge_env_strats[1]) {
    be <- as.environment(base)
    switch()
    new_env <- if (override) { new } else { base }
    new_keys <- as.environment(new_env) %>% ls(all.names = T)
}

int__standard_macro_args <- list(
    block,
    env = parent.frame(),
    .dots = as.list(substitute(list(...)))[-1L]
)

## int__class_macro_args <-

make_class <- function (...) {
    ## args <-
    args <- list(name)
    cl <- do.call('setClass', list(name))
    setClass(name, slots = slots, )
}

setClass("Condition",
         slots = list(pred = "function",
                      arity = "logical",
                      shortcircuit = "logical"))
setClass("Filter",
         slots = list(conditions = "list"),
         validity = function (object) {
             object@conditions %.% is("Condition") %>% unlist %>% all
         })

assert <- function (expr, msg) {
    sub_cond <- sub(expr)
    if (hasArg(msg)) {
        bquote(if (.(expr)) {
                   stop(.(msg))
               })
    }
    subbed <- sub(expr)
    cl <- bquote(stopifnot(isTRUE(.(expr))))
    eval.parent(cl)
}



## ### anonymous functions (working with magrittr)
int__closure_creator <- function (block, env = parent.frame()) {
    sb <- substitute(block)
    eval(sb, env)
}

curry <- function (fn, n = 1) {
    assert()
}

## 0-arg anonymous function
## closure_pure <- int__closure_creator function (block) {
##     env <- parent.frame()
##     bs <- substitute(block)
##     function () { eval(bs, env) }
## }

## 1-arg anonymous function with arg `.`
closure_ <- function (block, env = parent.frame()) {
    eval(bquote(function (.) { .(substitute(block)) }), env)
}

## `%&%` <- function (arglist, block) {
##     as <- substitute(arglist) %>% int__convert_named_arglist %>% as.pairlist
##     bs <- substitute(block)
##     eval.parent(do.call("function", list(as, bs)))
## }

## ## mapper_fun: (x) -> x
## map <- function (iterexpr, mapper_fun, pred = is.null) {
##     if (!match.fun(pred)(iterexpr)) { return(match.fun(mapper_fun)(iterexpr)) }
##     iterexpr
## }

## ## do_unless: () -> x
## recover <- function (iterexpr, do_unless, pred = is.null) {
##     if (match.fun(pred)(iterexpr)) { return(match.fun(do_unless)()) }
##     iterexpr
## }

## setClass("Mappable", slots = c(names = "character", values = "ANY"))

## is.empty <- function (x) {
##     len_p <- { length(.) == 0 }
##     pcase(x = x,
##         x,
##         is.list = .(len_p),
##         is.vector = .(len_p),
##         is.matrix = .(len_p))
## }

## convert_mappable <- function (x) {
##     pcase(is.list = pcase(., ))
## }

## `%.%` <- function (lhs, rhs) {
##     env <- parent.frame()
##     args <- as.list(substitute(list(...)))[-1L]
##     ev_args <- lapply(args)
##     print(args)
##     eval.parent(bquote(call('map', .(args))))
## }

## `%^%` <- function (lhs, rhs)

## make_extractor <- function (preds) {
##     b <- bquote(function (x) {
##         .(preds) %^% (match.fun %o% assert)
##         preds %T>% . %^% (match.fun %o% assert)
##         for (cur_pred in preds) {
##             match.fun(cur_pred)
##             assert((match.fun(cur_pred))(x))
##         }
##     })
## }

## match.language <- function (x) {
##     stopifnot(is.language(x))
##     x
## }

## match.errmsg <- function (x) {
##     assert(is.character(x) && (length(x) == 1))
##     x
## }

## assert <- function (expr, msg) {
##     sub_cond <- sub(expr)
##     if (hasArg(msg)) {
##         bquote(if (.(expr)) {
##                    stop(.(msg))
##                })
##     }
##     subbed <- sub(expr)
##     cl <- bquote(stopifnot(isTRUE(.(expr))))
##     eval.parent(cl)
## }

## get_names <- function (x) {
##     x %>% pcase(is.list = names,
##                 is.vector = names,
##                 is.data.frame = colnames)
## }

## pcase <- function (x, ..., default, env = parent.frame()) {
##     assert(hasArg(default))
##     args <- as.list(substitute(list(...)))[-1L]
##     nms <- names(args)
##     assert(((length(args) == 0) && is.null(names(args))) ||
##            (!any(nms == '')))
##     body <- if (length(args) >= 0) {
##                 stopifnot(hasArg(default))
##                 stopifnot(!is.null(nms))
##                 substitute(default)
##             } else {
##                 ## we have some predicates
##                 cur_pred <- match.fun(nms[[1]])
##                 if (cur_pred(x)) {
##                     match.language(args[[1]])
##                 } else {
##                     rest <- args[-1L]
##                     tail_args <- c(list(x = x),
##                                    args[-1L],)
##                     do.call('pcase', c(rest, default))
##                 }
##             }

## }

## `%^%` <- function (args, body) {
##     if (is.call(args)) {

##     }

## }

## `%o%` <- function (f, g) {
##     bquote(function (...) {
##         .(match.fun(f))(...)
##         f(...)
##     })
## }

## `%|%` <- function (...) {
##     args <- as.list(substitute(list(...)))[-1L]
##     eval.parent(do.call('or', args))
## }

## fold <- function (x, f, ...) { Reduce(f, x, ...) }

## filter <- function (x, f) { Filter(f, x) }

## `%!%` <- function (a, b) { filter(a, b) }
