SmartClose
==========

SmartClose is a Windows system tool that automates the task of closing all running programs. It can also save the state of the system to a snapshot so that all closed programs can be started again later with minimal effort. This repository contains the Delphi source code of SmartClose.

Let me start by admitting that the main part of the SmartClose source code is rubbish. I have written most of it in the early 2000s, before I had any real experience with coding and before I followed computer science studies at university. So I was blatantly unaware of good coding practices such as object-oriented programming. This becomes clear very quickly once you start browsing through the source code.

The reason, then, why I have put the source code online is for educational purposes; to show how SmartClose works under the hood and how certain things are done. Another reason is that people have asked me for the source code. If you have any plans for making major improvements to the program, then I would *strongly* suggest to just completely rewrite it. This might be a big task at first, but it would certainly give better results in the end than applying hack after hack on the current messy code.

Pull requests for minor improvements are always welcome, although I can't guarantee that they will ever be released in a new version. I have lost interest in working on SmartClose a long time ago and have moved on to other projects since.

-- Tim De Baets (@tdebaets)

Obtaining the source code
-------------------------

First make sure that you have a recent version of the [Git client](https://git-scm.com/) (`git`) installed. Then open a Windows command prompt window (note that Git Bash isn't supported). In the command prompt, run these commands:
```
> git clone https://github.com/tdebaets/smartclose.git smartclose
> cd smartclose
```

Finally, run the `postclone.bat` script. This will take care of further setting up the repository, installing Git hooks, creating output directories etc.:
```
> postclone.bat
```

To keep your repository up-to-date, run the `update.bat` script. This script essentially runs a `git pull` but also performs some basic checks before pulling. It also runs a `git submodule update` after the pull to update the `common` submodule as well.

If you want to contribute to this project, don't clone its main repository, but create your own fork first and clone that fork instead. Then commit/push your work on a topic branch and submit a pull request. For details, see the [generic instructions for contributing to projects](https://github.com/tdebaets/common/blob/master/CONTRIBUTING.md) in the `common` repository.

Building
--------

SmartClose has been written in Borland Delphi 4. This means that in order to build this project, you'll need to have Borland Delphi 4 installed and properly set up. See the [generic instructions for building Delphi projects](https://github.com/tdebaets/common/blob/master/Delphi/Building.md) in the `common` repository.

License
-------

SmartClose is Copyright © 2016-2017 Tim De Baets. It is licensed under the Apache License version 2.0, see [LICENSE](LICENSE) for details.
