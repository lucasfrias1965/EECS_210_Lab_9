# Lab 9

Hey Pardas,

Hope you're doing alright. So, basically, Fortran is the name of the game today. **Very suprisingly**, you already have a Fortran compilier installed. It's installed by default with gcc, which is installed on most Unix-esque systems in this planet.

## Quick Disclaimer

I used a different method than maybe was intended. I saw there was DFS or eigenvalue stuff, but I decided to just go ahead and do the nice and easy (A+I)^n-1. This is mostly because Fortran implements matrix multiplication in the more trendy modern versions (Fortran 90).

So I'm not using any external library, rather its a feature of the language itself. Hope that's oaky.



## Installation

Have Homebrew. I would be a little confused if you didn't already, but just in case

```
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

Now we've installed homebrew, you can use that to install gcc. You probably already also have gcc, but just in case again:

```
brew install gcc
```

Now you've installed gcc. You can use the built in gcc-fortran compilier, gfortran:

```
gfortran main.f90
./a.out
```

and you've done it!

