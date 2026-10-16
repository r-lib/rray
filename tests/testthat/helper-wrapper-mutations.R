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

    w <- rray_set_dimensions(x, c(10L, 10L))
    plain <- unserialize(serialize(w, NULL))

    expect_identical(f(w), f(plain), info = name)
    expect_identical(x, x_before, info = name)
    expect_identical(w, plain, info = name)
    expect_identical(
      obj_address(wrapper_data(w)),
      obj_address(x),
      info = name
    )
  }
}

mutation_cases_any <- function() {
  list(
    print = function(w) utils::capture.output(print(w)),
    format = function(w) format(w),
    str = function(w) utils::capture.output(utils::str(w)),
    summary = function(w) summary(w),
    head = function(w) utils::head(w),
    identical = function(w) identical(w, w),
    all_equal = function(w) all.equal(w, w),
    dput = function(w) utils::capture.output(dput(w)),
    serialize = function(w) unserialize(serialize(w, NULL)),
    rds = function(w) {
      path <- tempfile()
      on.exit(unlink(path))
      saveRDS(w, path)
      readRDS(path)
    },
    subset = function(w) w[1:5, 2],
    subset2 = function(w) w[[3]],
    split = function(w) split(w, rep(1:2, 50L)),
    match = function(w) match(w[1:3], w),
    in_op = function(w) w[1:3] %in% w,
    rev = function(w) rev(w),
    as_list = function(w) as.list(w),
    lapply = function(w) lapply(1:3, function(i) w[[i]]),
    do_call_c = function(w) do.call(c, list(w, w)),
    assign_elt = function(w) {
      y <- w
      y[1] <- y[2]
      y
    },
    assign_elt2 = function(w) {
      y <- w
      y[[2]] <- y[[3]]
      y
    },
    assign_all = function(w) {
      y <- w
      y[] <- y[1]
      y
    },
    assign_dim = function(w) {
      y <- w
      dim(y) <- c(20L, 5L)
      y
    },
    assign_dim_null = function(w) {
      y <- w
      dim(y) <- NULL
      y
    },
    assign_dimnames = function(w) {
      y <- w
      dimnames(y) <- list(letters[1:10], LETTERS[1:10])
      y
    },
    assign_attr = function(w) {
      y <- w
      attr(y, "foo") <- 1
      y
    },
    assign_class = function(w) {
      y <- w
      class(y) <- "foo"
      unclass(y)
    },
    assign_diag = function(w) {
      y <- w
      diag(y) <- y[1]
      y
    },
    single_binding = function(w) {
      local({
        y <- rray_set_dimensions(w, 100L)
        y[1] <- y[2]
        y
      })
    },
    rray = function(w) {
      rray_slice(rray_reverse(w, axes = 1L), 1:3, 1:2)
    }
  )
}

mutation_cases_missing <- function() {
  list(
    assign_is_na = function(w) {
      y <- w
      is.na(y) <- 3
      y
    }
  )
}

mutation_cases_numeric <- function() {
  list(
    plus = function(w) w + 1,
    plus_rev = function(w) 1 + w,
    times_self = function(w) w * w,
    negate = function(w) -w,
    power = function(w) w^2,
    modulo = function(w) w %% 3,
    int_divide = function(w) w %/% 3,
    equal = function(w) w == 1,
    temporary = function(w) (w + 0) + 1,
    abs = function(w) abs(w),
    sqrt = function(w) sqrt(abs(w)),
    exp = function(w) exp(w / 1000),
    round = function(w) round(w),
    floor = function(w) floor(w),
    sum = function(w) sum(w),
    mean = function(w) mean(w),
    range = function(w) range(w),
    var = function(w) stats::var(as.vector(w)),
    median = function(w) stats::median(w),
    quantile = function(w) stats::quantile(w),
    cumsum = function(w) cumsum(w),
    cumprod = function(w) cumprod(w / 100),
    cummax = function(w) cummax(w),
    sort = function(w) sort(w),
    sort_radix = function(w) sort(w, method = "radix"),
    sort_partial = function(w) sort(as.vector(w), partial = 5),
    order = function(w) order(w),
    rank = function(w) rank(w),
    unique = function(w) unique(as.vector(w)),
    duplicated = function(w) duplicated(w),
    table = function(w) table(w),
    which_max = function(w) which.max(w),
    find_interval = function(w) findInterval(as.vector(w), c(0, 50, 100)),
    cut = function(w) cut(w, 3),
    rle = function(w) rle(as.vector(w)),
    is_na = function(w) is.na(w),
    any_na = function(w) anyNA(w),
    transpose = function(w) t(w),
    aperm = function(w) aperm(w),
    matrix_multiply = function(w) w %*% w,
    crossprod = function(w) crossprod(w),
    solve = function(w) solve(w + diag(1000, 10)),
    qr = function(w) qr(w),
    svd = function(w) svd(w),
    det = function(w) det(w),
    chol = function(w) chol(crossprod(w) + diag(10)),
    apply = function(w) apply(w, 1, sum),
    sweep = function(w) sweep(w, 2, 1),
    scale = function(w) scale(w),
    row_sums = function(w) rowSums(w),
    col_means = function(w) colMeans(w),
    diag = function(w) diag(w),
    outer = function(w) outer(w[1:3], 1:3),
    kronecker = function(w) kronecker(w[1:2, 1:2], diag(2)),
    matrix = function(w) matrix(w, 20),
    array = function(w) array(w, c(5, 20)),
    as_vector = function(w) as.vector(w),
    as_integer = function(w) as.integer(w),
    as_character = function(w) as.character(w),
    c = function(w) c(w, 1),
    rbind = function(w) rbind(w, w),
    cbind = function(w) cbind(w, w),
    unlist = function(w) unlist(list(w, w)),
    vapply = function(w) vapply(1:10, function(i) as.double(w[i]), double(1)),
    ifelse = function(w) ifelse(w > 50, w, 0),
    pmin = function(w) pmin(w, 50),
    replace = function(w) replace(w, 1, 0),
    diff = function(w) diff(w),
    fft = function(w) stats::fft(w),
    filter = function(w) stats::filter(as.vector(w), rep(1, 3))
  )
}

mutation_cases_logical <- function() {
  list(
    not = function(w) !w,
    and = function(w) w & TRUE,
    xor = function(w) xor(w, TRUE),
    which = function(w) which(w),
    any = function(w) any(w, na.rm = TRUE),
    sum = function(w) sum(w, na.rm = TRUE),
    ifelse = function(w) ifelse(w, 1, 2),
    temporary = function(w) !(!w)
  )
}

mutation_cases_complex <- function() {
  list(
    re = function(w) Re(w),
    mod = function(w) Mod(w),
    conj = function(w) Conj(w),
    plus = function(w) w + 1i,
    sqrt = function(w) sqrt(w),
    fft = function(w) stats::fft(w),
    matrix_multiply = function(w) w %*% w,
    solve = function(w) solve(w + diag(1000, 10)),
    cumsum = function(w) cumsum(w)
  )
}

mutation_cases_raw <- function() {
  list(
    as_integer = function(w) as.integer(w),
    xor = function(w) xor(w, as.raw(1)),
    raw_to_char = function(w) rawToChar(w[1:10]),
    raw_shift = function(w) rawShift(w, 1)
  )
}

mutation_cases_character <- function() {
  list(
    toupper = function(w) toupper(w),
    nchar = function(w) nchar(w),
    paste0 = function(w) paste0(w, "x"),
    gsub = function(w) gsub("s", "t", w),
    sprintf = function(w) sprintf("%s!", w),
    strsplit = function(w) strsplit(w, ""),
    sort = function(w) sort(w),
    unique = function(w) unique(w),
    factor = function(w) factor(w),
    enc2utf8 = function(w) enc2utf8(w),
    trimws = function(w) trimws(w),
    starts_with = function(w) startsWith(w, "s1"),
    chartr = function(w) chartr("s", "t", w),
    assign_substr = function(w) {
      y <- w
      substr(y, 1, 1) <- "z"
      y
    },
    assign_encoding = function(w) {
      y <- w
      Encoding(y) <- "UTF-8"
      y
    }
  )
}

mutation_cases_list <- function() {
  list(
    lapply = function(w) lapply(w, function(elt) elt + 1L),
    unlist = function(w) unlist(w),
    rapply = function(w) rapply(w, function(elt) elt * 2L, how = "list"),
    sapply = function(w) sapply(w, identity),
    vapply = function(w) vapply(w, function(elt) elt + 0L, integer(1)),
    mapply = function(w) mapply(function(a, b) a + b, w, w),
    do_call = function(w) do.call(sum, as.list(w)),
    assign_elt_type = function(w) {
      y <- w
      y[[1]] <- "z"
      y
    },
    assign_null = function(w) {
      y <- w
      y[2] <- list(NULL)
      y
    }
  )
}
