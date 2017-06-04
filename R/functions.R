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
    expr_quot <- substitute(expr)
    expr_used <- if (evalExpr) { eval(expr_quot, env) } else { expr_quot }
    sub_env <- coerce_env(subs)
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
    if (len == 0) { return(alist()) }
    names <- names(lst)
    ## if called with only values, names is character(0) instead of blanks
    if (length(names) == 0) {
        names <- rep('', len)
    }
    have_names <- names[names != '']
    ret <- alist_name_only(names)
    for (i in 1:len) {
        name <- names[[i]]
        val <- lst[[i]]
        if (name == '' && swapNameless) {
            names(ret)[[i]] <- as.character(val)
        } else {
            if (parse) { val <- parse(text = val) }
            if (eval) { val <- eval(val, env) }
            ret[[i]] <- val
            if (name != '') {
                names(ret)[[i]] <- name
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

macro_extra_args <- list(
    env = quote(parent.frame())
)

macro_expr_args <- list(
    quote(expr),
    expr_q = quote(substitute(expr))
)

make_arglist <- function (...) {
    to_alist(sapply(match.call(expand.dots = T)[-1], deparse),
             parse = T, eval = T)
}

make_arglist_binding <- as.name("l")

macro_fun <- function (args, defn,
                       enclos = env_with(),
                       to_sub = list(),
                       with_expr = T,
                       extra_args = macro_extra_args) {
    def_q <- substitute(defn)
    args_q <- substitute(args)
    args_env <- env_with(join_to_named_list(list("l", make_arglist)),
                         env = parent.frame())
    given_args_alist <- eval(args_q, args_env)
    args_all <- c(given_args_alist, to_alist(extra_args))
    if (with_expr) {
        args_all <- c(args_all, to_alist(macro_expr_args))
    }
    body_q <- sub_in_expr(to_sub, def_q, evalExpr = T)
    result_q <- call("function", as.pairlist(args_all), body_q)
    eval(result_q, enclos)
}



### Functions real users might want.

anon_one_argname <- as.name(".")

anon_one <- function (expr,
                      evalExpr = TRUE, argname = anon_one_argname,
                      env = parent.frame()) {
    expr_quot <- substitute(expr)
    expr_used <- if (evalExpr) { expr_quot } else { eval(expr_quot, env) }
    subs_dict <- join_to_named_list(list(argname, argname))
    arg_list <- as.pairlist(alist_name_only(argname))
    body_quot <- sub_in_expr(subs_dict, expr_used)
    result_quot <- call("function", arg_list, body_quot)
    eval(result_quot, env)
}
