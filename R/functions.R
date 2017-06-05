###
### functions -- expressive function and macro creation
## Copyright (C) 2017 Danny McClanahan
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.



### Utility functions

coerce_env <- function (obj) {
    if (is.environment(obj)) { obj }
    else if (is.list(obj)) { list2env(obj) }
    else {
        stop(sprintf(
            "object '%s' could not be converted to an environment", obj))
    }
}

sub_in_expr <- function (subs, expr,
                         evalExpr = FALSE, env = parent.frame()) {
    subs_q <- substitute(subs)
    expr_quot <- substitute(expr)
    expr_used <- if (evalExpr) { eval(expr_quot, env) } else { expr_quot }
    sub_env <- coerce_env(eval(subs_q, env))
    eval(bquote(substitute(.(expr_used), sub_env)))
}

alist_name_only <- function (names) {
    len <- length(names)
    setNames(rep(alist(,)[1], len), lapply(names, as.character))
}

alist_has_value <- function (alist_slice) {
    deparse(alist_slice[[1]]) != ''
}

## NOTE: accepts lists too!
invert_alist <- function (al) {
    len <- length(al)
    ret <- alist_name_only(rep("", len))
    names <- names(al)
    for (i in 1:length(al)) {
        names(ret)[[i]] <-
            if (alist_has_value(al[i])) {
                as.character(al[[i]])
            } else { '' }
        name <- names[[i]]
        if (name != '') {
            ret[[i]] <- name
        }
    }
    ret
}

to_alist <- function (lst, swapNameless = T, parse = F, eval = F,
                      env = parent.frame()) {
    len <- length(lst)
    print(sprintf("lst = '%s'", lst))
    if (len == 0) { return(alist()) }
    names <- names(lst)
    ## if called with only values, names is character(0) instead of blanks
    if (length(names) == 0) {
        names <- rep('', len)
    }
    print(sprintf("names: '%s'", names))
    ret <- alist_name_only(names)
    for (i in 1:len) {
        name <- names[[i]]
        if (!alist_has_value(lst[i])) {
            names(ret)[[i]] <- name
        } else {
            val <- lst[[i]]
            print(name)
            print(val)
            if (parse) { val <- parse(text = val) }
            if (eval) { val <- eval(val, env) }
            if (name == '' && swapNameless) {
                stopifnot(is.character(val))
                names(ret)[[i]] <- as.character(val)
            } else {
                ret[[i]] <- val
                if (name != '') {
                    names(ret)[[i]] <- name
                }
            }
        }
    }
    ret
}

join_to_named_list <- function (flattened_pairs) {
    len <- length(flattened_pairs)
    stopifnot(len %% 2 == 0)
    names <- list()
    values <- list()
    for (i in seq(1, len, by = 2)) {
        names <- c(as.character(flattened_pairs[[i]]), names)
        values <- c(flattened_pairs[[i + 1]], values)
    }
    setNames(rev(values), rev(names))
}

env_with <- function (bindings = list(), env = parent.frame()) {
    bind_list <- if (is.list(bindings)) {
                     bindings
                 } else if (is.environment(bindings)) {
                     as.list(bindings, all.names = T)
                 } else {
                     stop(sprintf("bindings '%s' must be list or environment",
                                  bindings))
                 }
    ret <- as.environment(c(as.list(bind_list, as.list(env, all.names = T))))
    parent.env(ret) <- parent.env(env)
    ret
}

macro_extra_args <- to_alist(list(
    env = quote(parent.frame())
))

macro_expr_name <- "expr"
macro_expr_q_name <- "expr_q"

macro_expr_args <- to_alist(c(
    macro_expr_name,
    join_to_named_list(list(macro_expr_q_name, quote(substitute(expr))))
))

make_arglist <- function (...) {
    to_alist(sapply(match.call(expand.dots = T)[-1], deparse),
             parse = T, eval = T)
}

join_arglists <- function (...) {
    all <- to_alist(c(...))
    all_names <- names(all)
    len <- length(all)
    positional <- alist_name_only(rep("", len))
    positional_n <- 0
    named <- alist_name_only(rep("", len))
    named_n <- 0
    names_used <- c()
    for (i in 1:len) {
        name <- all_names[[i]]
        stopifnot(!(name %in% names_used))
        names_used <- c(name, names_used)
        if (alist_has_value(all[i])) {
            named_n <- named_n + 1
            names(named)[[named_n]] <- name
            named[[named_n]] <- all[[i]]
        } else {
            positional_n <- positional_n + 1
            names(positional)[[positional_n]] <- name
        }
    }
    pos_args <- positional[1:positional_n]
    named_args <- named[1:named_n]
    c(pos_args, named_args)
}

make_arglist_binding <- as.name("l")

make_macro <- function (args, defn,
                        enclos = env_with(),
                        with_expr = T,
                        extra_args = macro_extra_args) {
    def_q <- substitute(defn)
    args_q <- substitute(args)
    args_env <- env_with(
        join_to_named_list(list(make_arglist_binding, make_arglist)),
        env = parent.frame())
    given_args_alist <- eval(args_q, args_env)
    args_all <- c(given_args_alist, to_alist(extra_args))
    if (with_expr) {
        args_all <- c(args_all, to_alist(macro_expr_args))
    }
    arg_list <- as.pairlist(join_arglists(args_all))
    result_q <- call("function", arg_list, def_q)
    eval(result_q, enclos)
}

macro_fun <- function (args, fargs, ...) {
    fargs_q <- substitute(fargs)
    result_q <- bquote(make_macro(args, {
        fargs_ev <- eval(.(fargs_q))
        arg_list <- as.pairlist(fargs_ev)
        result_q <- call("function", arg_list, expr_q)
        eval(result_q, env)
    }, enclos = parent.frame(), ...))
    eval(result_q)
}



### Functions real users might want.

anon_one_argname <- as.name(".")

## anon_one <- macro_fun(
##     l(name = anon_one_argname), to_alist(list(name))
## )
