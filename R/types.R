###
### types -- a flexible type system
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


library(magrittr)



## setClass("Monad")
## setClass("Option", representation("val"), contains = "Monad")
## setClass("Sequence", contains = "Option",
##          validity = m_s())
base_hierarchy <- list()

## setClass(class = "TypeRepr",
##          slots = list(name = "character",
##                       templates = "character",
##                       parents = "character"))

## mangle_type_nest <- function (type_repr) {
##     sprintf(
##         "%s<%s>:%s",
##         type_repr$name,
##         paste(collapse = ',', type_repr$templates),
##         paste(collapse = ',', type_repr$parents))
## }

## get_type <- function (name, templates = list(), parents = list()) {
##     repr <-
##     setClass()
## }



m_z <- function (body) {
    eval.parent(call('function', as.pairlist(alist()), substitute(body)))
}

m_s <- function (body) {
    eval.parent(call('function', as.pairlist(alist(. =)), substitute(body)))
}

m_map <- function (expr, block, test = )

m_or <- function (expr, block, test = is.null) {
    res <- eval.parent(substitute(expr))
    pred <- eval.parent(substitute(test))
    cond <-
        if (is.function(pred)) {
            isTRUE(pred(res))
        } else if () {

        }
    f <- match.fun(test)
    if (isTRUE(f(res))) {
        eval(substitute(block), env)
    } else {
        res
    }
}

m_when <- function (..., test = Negate(is.null)) {
    m_or(..., test = test)
}

m_marshal_fun_arg <- function (obj) {
    os <- substitute(obj)
    if (is.function(obj)) {
        obj
    } else if (is.call(obj)) {
        to_call <- obj %>%
            as.list %>%
            { c(.[[1]], as.name("."), .[-1L]) } %>%
            as.call
        function (.) {
            eval(to_call)
        }
    } else {
        stop("?")
    }
}

gen_names <- function (x) { names(x) %>% m_or(rep('', length(x))) }

m_app <- function (iterable, body, unlist = TRUE) {
    bs_f <- eval.parent(substitute(m_s(body)))
    lapply(iterable, bs_f) %>%
        m_or(., unlist(.), test = eval.parent(m_s(unlist)))
}

m_nm_app <- function (iterable, body, ...) {
    ## like `m_app`, but `body` is evaluated with `.` set to the value of the
    ## list element, and `names(.)` is a 1-element vector containing the name of
    ## the list element, or `NULL` if it has no name
    bsub <- substitute(body)
    nms <- names(iterable) %>% m_or()
    n <- length(iterable)
    m_app(1:n,
        {

        },
        ...)
}

m_fold <- function (x, f, init, ...) {
    if (hasArg(init)) {
        Reduce(f, x, init, ...)
    } else {
        Reduce(f, x, ...)
    }
}

transform_arglist <- function (...) {
    clauses <- substitute(list(...)) %>% as.list %>% .[-1L]
    n <- length(clauses)
    nms <- gen_names(clauses)
    name_only <- nms == ''
    nms[name_only] <- clauses[name_only] %>% as.character
    output <- rep(alist(. = ), n) %>% setNames(nm = nms)
    output[!name_only] <- clauses[!name_only]
    output
}

m_args <- function (...) { transform_arglist(...) }

`%&%` <- function (arglist, body) {
    env <- parent.frame()
    ## `arglist` is the result of `m_a`()
    gen_call <- call('function', as.pairlist(arglist), substitute(body))
    eval(gen_call, env)
}

## m_partition_by <- function (iterable, transformer) {
##     f <- match.fun(transformer)
##     m_fold(iterable, init = hashmap(), m_args(., cur) %&% {
##         .$find(cur) %>% m_or(test = is.na, list()) %>%
##     })
##     map <- ()
##     Reduce(x = iterable, init = hashmap(), f = )
## }

## Local Variables:
## ess-r-package-info: ("acceleRated" . "/home/cosmicexplorer/tools/acceleRated/")
## End:
