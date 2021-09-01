# Installing the tstools package

I created the [tstools package](https://bitbucket.org/bachmeil/tstools)
to polish some of the rough edges associated with time series 
analysis in R. The package is not available through CRAN, so don't
try to install it using `install.packages`. In fact, there was a different
tstools package on CRAN (first uploaded in 2018, many years after the first
release of my package) that is no longer there. If you install that
package - which has nothing to do with mine - your programs will not run.

You need to use the devtools package to install tstools. First
install devtools in the usual way:

```
install.packages("devtools")
```

Once that's installed, load devtools and call `install_bitbucket`:

```
library(devtools)
install_bitbucket("bachmeil/tstools")
```

That is it. You can test your installation by checking if the following code
runs and gives the expected output:

```
library(tstools)
x <- ts(1:10)
lags(x, 1:2)
```

The output should look like this:

```
Time Series:
Start = 3 
End = 11 
Frequency = 1 
   xL1 xL2
 3   2   1
 4   3   2
 5   4   3
 6   5   4
 7   6   5
 8   7   6
 9   8   7
10   9   8
11  10   9
```

## Warning

Do not include the `install_bitbucket` line in your program!!!

Doing that will make your program run slowly, because you'll be downloading and
installing the package every time you run your program. (Or at least you'll be
repeatedly checking for an update.) You'll also be
wasting lots of bandwidth downloading the package for no reason. **Once the
package is installed, you don't need to install it again unless a new
version has been released and you want to upgrade to it.**

I once had a student tell me he avoided tstools because it made his
program run very slowly. It took hours to do what should have required at most
a few seconds. 

Not only was he calling `install_bitbucket` in his program for no reason, but he was doing it *inside a loop* with option `force=TRUE`. The code on the inside of the loop only took a fraction of a second to run, but it was pretty slow to
download and install the package on every iteration.

*Last updated: August 25, 2021*
