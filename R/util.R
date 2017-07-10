### Utility routines.
## Written by Danny McClanahan, Irish Lab June 2017.
## <danieldmcclanahan@gmail.com>



### Libraries loaded.
library(magrittr)



### Type checking and assertions.
assert <- function (expr, msg, expected = TRUE, test = identical) {
    se <- substitute(expr)
    res <- eval.parent(se)
    if (!match.fun(test)(expected, res)) {
        ## failed
        if (!hasArg(msg)) {
            ex_s <- deparse(expr)
            msg <- sprintf(
                paste("the expression '%s' evaluated to '%s',",
                      "but '%s' was expected (with test '%s')"),
                ex_s, res, expected, test)
            stopifnot(isTRUE(is.character(msg)))
        }
        stop(msg)
    }
    res
}

closure_pure <- function (block) {
    bs <- substitute(block)
    eval.parent(bs)
}

closure_one <- function (block, nm = ".") {
    bs <- substitute(block)
    argl <- .convert_named_arglist(list(nm))
    ret <- list(as.pairlist(argl), bs)
    eval.parent(do.call('function', ret))
}

.convert_named_arglist <- function (x) {
    n <- length(x)
    alt <- as.list(x)
    nms <- names(alt)
    if (is.null(nms)) { nms <- rep('', n) }
    name_only <- (nms == '')
    objs <- rep(alist(. =), n)
    objs[!name_only] <- alt[!name_only]
    nms[name_only] <- alt[name_only]
    setNames(objs, unlist(nms))
}

.convert_callable <- function (x,
                               arglist = .convert_named_arglist(list(".")),
                               env = parent.frame(),
                               vectorize = TRUE) {
    sx <- substitute(x)
    conv <- if (is.language(sx)) {
                b <- bquote(
                    do.call('function', list(.(as.pairlist(arglist)), .(sx))))
                eval(b, env)
            } else if (is.function(x)) {
                match.fun(x)
            } else {
                x
            }
    if (vectorize) { Vectorize(conv) } else { conv }
}

.update_env <- function (base, new, nms) {
    gen <- new.env(parent = base)
    for (nm in nms) {
        assign(nm, get(nm, new), gen)
    }
    gen
}

`%&%` <- function (arglist, block) {
    as <- arglist %>% .convert_named_arglist %>% as.pairlist
    bs <- substitute(block)
    eval.parent(do.call("function", list(as, bs)))
}

.conf_list <- function (.) {
    argl <- .convert_named_arglist(list("x", "y")) %>% as.pairlist
    .convert_callable(., arglist = argl)
}

.merge_env_strats <- .conf_list(
    quote(list(
        left = setdiff(y, x),
        right = setdiff,
        both = intersect,
        either = union,
        neither = function (x, y) {
            ld <- setdiff(x, y)
            rd <- setdiff(y, x)
            intersect(ld, rd)
        })))


merge_env <- function (base, new, strategy = .merge_env_strats[[1]]) {
    b_env <- as.environment(base)
    n_env <- as.environment(new)
    f <- match.fun(strategy) %>% .convert_callable
    .update_env(b_env, n_env, f(names(b_env), names(n_env)))
}

.make_standard_macro <- function (block, env = parent.frame(),
                                  arglist = .convert_named_arglist(list(".")),
                                  ...) {
    args <- as.list(substitute(list(...)))[-1L]
    bs <- substitute(block)
    print(bs)
    .convert_callable(
        bs, .convert_named_arglist(list(".", "env", "arglist", "...")))
}

## make_class <- function (...) {
##     ## args <-
##     args <- list(name)
##     cl <- do.call('setClass', list(name))
##     setClass(name, slots = slots, )
## }

## setClass("Condition",
##          slots = list(pred = "function",
##                       arity = "logical",
##                       shortcircuit = "logical"))
## setClass("Filter",
##          slots = list(conditions = "list"),
##          validity = function (object) {
##              object@conditions %.% is("Condition") %>% unlist %>% all
##          })


## ### anonymous functions (working with magrittr)
## .closure_creator <- function (block, env = parent.frame()) {
##     sb <- substitute(block)
##     eval(sb, env)
## }

## curry <- function (fn, n = 1) {
##     assert()
## }

## 0-arg anonymous function
## closure_pure <- .closure_creator function (block) {
##     env <- parent.frame()
##     bs <- substitute(block)
##     function () { eval(bs, env) }
## }

## 1-arg anonymous function with arg `.`
## closure_ <- function (block, env = parent.frame()) {
##     eval(bquote(function (.) { .(substitute(block)) }), env)
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

## match.blockuage <- function (x) {
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

## Local Variables:
## ess-r-package-info: ("acceleRated" . "/home/cosmicexplorer/tools/acceleRated/")
## End:
