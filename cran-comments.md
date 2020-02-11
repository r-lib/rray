## Test environments
* local OS X install, R 3.6.0
* ubuntu 14.04 (on travis-ci), R 3.6.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## 0.1.1 Resubmission

This is a resubmission of rray that attempts to fix the detected runtime errors detected in the original 0.1.1 submission. There is still a separate GCC 10 warning that comes from xtensor, but they are convinced that it is not an issue. Specifically it is:

> `warning: array subscript 1 is outside array bounds of 'std::size_t [1]'`

This is my conversation with them:
https://github.com/xtensor-stack/xtensor/issues/1909

## 0.1.1 Submission

This is a patch release of rray to be compatible with vctrs 0.2.2.

## 0.1.0 Resubmission

This is a resubmission of rray based on the following CRAN comments.

```
Thanks, please do not comment out your examples and use \donttest{} instead:

\examples{
    examples for users and checks:
    executable in < 5 sec

    donttest{
        further examples for users (not used for checks)
    }
}
```

I have removed the commented out examples.

```
You have examples for unexported functions which cannot run in this way.
Please either add rray::: to the function calls in the examples, omit 
these examples or port these functions.
```

I believe these were related to unexported functions that I still wanted to
document. I no longer generate Rd for these, so this should be fixed.

```
Missing Rd-tags:
  as_array.Rd: \value
  as_matrix.Rd: \value
  as_rray.Rd: \value
....

Please add this tag in your Rd-files and explain the function's output.
```

I have added return value descriptions for all functions.

## 0.1.0 Submission

This is the first release of rray.

The install size of rray is on the larger end for an R package, but is 
generally due to xtensor being a header only library.
