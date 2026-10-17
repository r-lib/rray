mutation_vector <- function(type) {
  switch(
    type,
    logical = rep(c(TRUE, FALSE, NA, TRUE), 25L),
    integer = 1:100 + 0L,
    double = as.double(1:100) + 0.5,
    complex = complex(real = 1:100, imaginary = 100:1),
    raw = as.raw(1:100),
    character = paste0("s", 1:100),
    list = as.list(1:100 + 0L),
    altrep_integer = 1:100,
    altrep_character = as.character(1:100)
  )
}

mutation_cases <- function(type) {
  cases <- switch(
    type,
    logical = mutation_cases_logical(),
    integer = mutation_cases_numeric(),
    double = mutation_cases_numeric(),
    complex = mutation_cases_complex(),
    raw = mutation_cases_raw(),
    character = mutation_cases_character(),
    list = mutation_cases_list(),
    altrep_integer = mutation_cases_numeric(),
    altrep_character = mutation_cases_character()
  )

  cases <- c(cases, mutation_cases_any())

  if (type != "raw") {
    cases <- c(cases, mutation_cases_missing())
  }

  cases
}

expect_no_mutation <- function(type) {
  cases <- mutation_cases(type)

  for (name in names(cases)) {
    f <- cases[[name]]

    x <- mutation_vector(type)
    x_before <- unserialize(serialize(x, NULL))

    v <- rray_set_dimensions(x, c(10L, 10L))
    plain <- unserialize(serialize(v, NULL))

    expect_identical(f(v), f(plain), info = name)
    expect_identical(x, x_before, info = name)
    expect_identical(v, plain, info = name)
    expect_identical(
      obj_address(view_data(v)),
      obj_address(x),
      info = name
    )
  }
}

mutation_cases_any <- function() {
  list(
    print = function(v) utils::capture.output(print(v)),
    format = function(v) format(v),
    str = function(v) utils::capture.output(utils::str(v)),
    summary = function(v) summary(v),
    head = function(v) utils::head(v),
    identical = function(v) identical(v, v),
    all_equal = function(v) all.equal(v, v),
    dput = function(v) utils::capture.output(dput(v)),
    serialize = function(v) unserialize(serialize(v, NULL)),
    rds = function(v) {
      path <- tempfile()
      on.exit(unlink(path))
      saveRDS(v, path)
      readRDS(path)
    },
    subset = function(v) v[1:5, 2],
    subset2 = function(v) v[[3]],
    split = function(v) split(v, rep(1:2, 50L)),
    match = function(v) match(v[1:3], v),
    in_op = function(v) v[1:3] %in% v,
    rev = function(v) rev(v),
    as_list = function(v) as.list(v),
    lapply = function(v) lapply(1:3, function(i) v[[i]]),
    do_call_c = function(v) do.call(c, list(v, v)),
    assign_elt = function(v) {
      y <- v
      y[1] <- y[2]
      y
    },
    assign_elt2 = function(v) {
      y <- v
      y[[2]] <- y[[3]]
      y
    },
    assign_all = function(v) {
      y <- v
      y[] <- y[1]
      y
    },
    assign_dim = function(v) {
      y <- v
      dim(y) <- c(20L, 5L)
      y
    },
    assign_dim_null = function(v) {
      y <- v
      dim(y) <- NULL
      y
    },
    assign_dimnames = function(v) {
      y <- v
      dimnames(y) <- list(letters[1:10], LETTERS[1:10])
      y
    },
    assign_attr = function(v) {
      y <- v
      attr(y, "foo") <- 1
      y
    },
    assign_class = function(v) {
      y <- v
      class(y) <- "foo"
      unclass(y)
    },
    assign_diag = function(v) {
      y <- v
      diag(y) <- y[1]
      y
    },
    single_binding = function(v) {
      local({
        y <- rray_set_dimensions(v, 100L)
        y[1] <- y[2]
        y
      })
    },
    rray = function(v) {
      rray_slice(rray_reverse(v, axes = 1L), 1:3, 1:2)
    }
  )
}

mutation_cases_missing <- function() {
  list(
    assign_is_na = function(v) {
      y <- v
      is.na(y) <- 3
      y
    }
  )
}

mutation_cases_numeric <- function() {
  list(
    plus = function(v) v + 1,
    plus_rev = function(v) 1 + v,
    times_self = function(v) v * v,
    negate = function(v) -v,
    power = function(v) v^2,
    modulo = function(v) v %% 3,
    int_divide = function(v) v %/% 3,
    equal = function(v) v == 1,
    temporary = function(v) (v + 0) + 1,
    abs = function(v) abs(v),
    sqrt = function(v) sqrt(abs(v)),
    exp = function(v) exp(v / 1000),
    round = function(v) round(v),
    floor = function(v) floor(v),
    sum = function(v) sum(v),
    mean = function(v) mean(v),
    range = function(v) range(v),
    var = function(v) stats::var(as.vector(v)),
    median = function(v) stats::median(v),
    quantile = function(v) stats::quantile(v),
    cumsum = function(v) cumsum(v),
    cumprod = function(v) cumprod(v / 100),
    cummax = function(v) cummax(v),
    sort = function(v) sort(v),
    sort_radix = function(v) sort(v, method = "radix"),
    sort_partial = function(v) sort(as.vector(v), partial = 5),
    order = function(v) order(v),
    rank = function(v) rank(v),
    unique = function(v) unique(as.vector(v)),
    duplicated = function(v) duplicated(v),
    table = function(v) table(v),
    which_max = function(v) which.max(v),
    find_interval = function(v) findInterval(as.vector(v), c(0, 50, 100)),
    cut = function(v) cut(v, 3),
    rle = function(v) rle(as.vector(v)),
    is_na = function(v) is.na(v),
    any_na = function(v) anyNA(v),
    transpose = function(v) t(v),
    aperm = function(v) aperm(v),
    matrix_multiply = function(v) v %*% v,
    crossprod = function(v) crossprod(v),
    solve = function(v) solve(v + diag(1000, 10)),
    qr = function(v) qr(v),
    svd = function(v) svd(v),
    det = function(v) det(v),
    chol = function(v) chol(crossprod(v) + diag(10)),
    apply = function(v) apply(v, 1, sum),
    sweep = function(v) sweep(v, 2, 1),
    scale = function(v) scale(v),
    row_sums = function(v) rowSums(v),
    col_means = function(v) colMeans(v),
    diag = function(v) diag(v),
    outer = function(v) outer(v[1:3], 1:3),
    kronecker = function(v) kronecker(v[1:2, 1:2], diag(2)),
    matrix = function(v) matrix(v, 20),
    array = function(v) array(v, c(5, 20)),
    as_vector = function(v) as.vector(v),
    as_integer = function(v) as.integer(v),
    as_character = function(v) as.character(v),
    c = function(v) c(v, 1),
    rbind = function(v) rbind(v, v),
    cbind = function(v) cbind(v, v),
    unlist = function(v) unlist(list(v, v)),
    vapply = function(v) vapply(1:10, function(i) as.double(v[i]), double(1)),
    ifelse = function(v) ifelse(v > 50, v, 0),
    pmin = function(v) pmin(v, 50),
    replace = function(v) replace(v, 1, 0),
    diff = function(v) diff(v),
    fft = function(v) stats::fft(v),
    filter = function(v) stats::filter(as.vector(v), rep(1, 3))
  )
}

mutation_cases_logical <- function() {
  list(
    not = function(v) !v,
    and = function(v) v & TRUE,
    xor = function(v) xor(v, TRUE),
    which = function(v) which(v),
    any = function(v) any(v, na.rm = TRUE),
    sum = function(v) sum(v, na.rm = TRUE),
    ifelse = function(v) ifelse(v, 1, 2),
    temporary = function(v) !(!v)
  )
}

mutation_cases_complex <- function() {
  list(
    re = function(v) Re(v),
    mod = function(v) Mod(v),
    conj = function(v) Conj(v),
    plus = function(v) v + 1i,
    sqrt = function(v) sqrt(v),
    fft = function(v) stats::fft(v),
    matrix_multiply = function(v) v %*% v,
    solve = function(v) solve(v + diag(1000, 10)),
    cumsum = function(v) cumsum(v)
  )
}

mutation_cases_raw <- function() {
  list(
    as_integer = function(v) as.integer(v),
    xor = function(v) xor(v, as.raw(1)),
    raw_to_char = function(v) rawToChar(v[1:10]),
    raw_shift = function(v) rawShift(v, 1)
  )
}

mutation_cases_character <- function() {
  list(
    toupper = function(v) toupper(v),
    nchar = function(v) nchar(v),
    paste0 = function(v) paste0(v, "x"),
    gsub = function(v) gsub("s", "t", v),
    sprintf = function(v) sprintf("%s!", v),
    strsplit = function(v) strsplit(v, ""),
    sort = function(v) sort(v),
    unique = function(v) unique(v),
    factor = function(v) factor(v),
    enc2utf8 = function(v) enc2utf8(v),
    trimws = function(v) trimws(v),
    starts_with = function(v) startsWith(v, "s1"),
    chartr = function(v) chartr("s", "t", v),
    assign_substr = function(v) {
      y <- v
      substr(y, 1, 1) <- "z"
      y
    },
    assign_encoding = function(v) {
      y <- v
      Encoding(y) <- "UTF-8"
      y
    }
  )
}

mutation_cases_list <- function() {
  list(
    lapply = function(v) lapply(v, function(elt) elt + 1L),
    unlist = function(v) unlist(v),
    rapply = function(v) rapply(v, function(elt) elt * 2L, how = "list"),
    sapply = function(v) sapply(v, identity),
    vapply = function(v) vapply(v, function(elt) elt + 0L, integer(1)),
    mapply = function(v) mapply(function(a, b) a + b, v, v),
    do_call = function(v) do.call(sum, as.list(v)),
    assign_elt_type = function(v) {
      y <- v
      y[[1]] <- "z"
      y
    },
    assign_null = function(v) {
      y <- v
      y[2] <- list(NULL)
      y
    }
  )
}
