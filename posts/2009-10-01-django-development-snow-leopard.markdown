---
title: Django Development on Snow Leopard
description: Installing a development environment for Django on Snow Leopard by using virtual environments
date: 1 October, 2009
---
__Update June, 2011__: Development on Snow Leopard just got a heck of a lot easier with the [homebrew] package. I'm going to give you a short walkthrough. First install homebrew by executing the following command in your shell:

	ruby -e "$(curl -fsSL https://raw.github.com/gist/323731)"

Next, [install XCode] from Apple and then [pip] for python package management.

	sudo easy_install pip

Install [Mercurial] and [Git] with the following commands:

	sudo pip install mercurial
	brew install git

[Virtualenv] and [Virtualenwrapper] can be installed with the following commands:

	sudo pip install virtualenv
	sudo pip install virtualenvwrapper

Make sure to put the following in your shells configuration file, usually ``~/.bash_profile`` or ``~/.zshrc``:

	# Virtualenvwrapper
	if [ -e /usr/local/bin/virtualenvwrapper.sh ]; then
     export WORKON_HOME=$HOME/.virtualenvs
      source /usr/local/bin/virtualenvwrapper.sh
	fi

This will keep al your virtual enviroments in the ``~/.virtualenvs`` path. That's all there is to it these days. Django development just got a lot easier! You can read more about how to use these tools at the bottom of this page "[Combining all the tools]" 

[homebrew]: http://mxcl.github.com/homebrew/
[install XCode]: http://developer.apple.com/technologies/xcode.html
[Combining all the tools]: #combining "Combing all the tools"

## Introduction

I got my Snow Leopard in the mail lately and I decided to do a format before installing it. I had also read a lot about the _new_ way of developing Django applications inside it's own virtual environment. And installing Python packages with PIP inside of those environments. The clean install of Snow Leopard gave me another reason to start using all this new technology. In this article I will give you a quick guide into creating a great Django development environment making use of the following software:

* [Mercurial](http://mercurial.selenic.com/wiki/)
* [Git](http://git-scm.com/)
* [PIP](http://pypi.python.org/pypi/pip)
* [Virtualenv](http://pypi.python.org/pypi/virtualenv)
* [Virtualenvwrapper](http://www.doughellmann.com/projects/virtualenvwrapper/)
* [Imaging Library](http://www.pythonware.com/products/pil/) (with JPEG and Freetype support)

When everything is installed I will describe in short how we develop our Django site's at [Bread & Pepper](http://breadandpepper.com) with all these shiny new tools.

## XCode

Some of the above software must be compiled and therefore we need to install XCode. You can find XCode on your Snow Leopard DVD or at the Apple developers website. Installing XCode is really straight forward and I expect that everyone that is interested in this kind of articles knows how to install it.

## Mercurial

We will first install Mercurial for downloading the most recent versions of some of the Python packages that are on [Bitbucket](http://bitbucket.org). We won't use MacPorts but just compile it by hand.

Let's launch the Terminal from your ''Applications'' folder and start with editing your `.profile` so you have `/url/local/bin` on your `$PATH`. I'm using Vim, so I will edit it by typing `vim .profile` inside the terminal and add the following code:

  export PATH="/usr/local/bin:/usr/local/sbin:$PATH"
  export PYTHONPATH=/usr/local/lib/python2.6/site-packages

Save and close the file (`:wq` or `ZZ` in Vim) and apply the new changes bytyping `source ~/.profile` in your terminal.

We can now download and install Mercurial. First download the source in your sources directory. Mine is `/Sources` (`mkdir ~/Sources` if you want the same).

    cd ~/Sources
    curl -O http://mercurial.selenic.com/release/mercurial-1.3.1.tar.gz

And finally let's build and install.

    tar xzvf mercurial-1.3.1.tar.gz
    cd mercurial-1.3.1#
    make all
    sudo make install
    cd ..

We can check if Mercurial got installed successfully by typing `which hg`. You should get `/usr/local/bin/hg`.

## Git

Now let's install Git, also needed for some packages that are on
[Github](http://github.com). Go to the `~/Sources` folder again and do the
following:

    curl -O http://kernel.org/pub/software/scm/git/git-1.6.4.4.tar.bz2
    tar xzvf git-1.6.4.2.tar.bz2
    cd git-1.6.4.4
    ./configure --prefix=/usr/local
    make
    sudo make install
    cd ..

Verify it installed correctly with ``which git``. You should see
``/usr/local/bin/git``.

## PIP

PIP is a Python package manager that works well with Virtualenv because you can say in which ''Virtual Environment'' you want to install the package. You can also supply PIP with an `requirements` file which lists all the packages used in the project. More abouth this later, let's install it first

    sudo easy_install pip==dev

## Virtualenv and Virtualenvwrapper

Virtualenv supplies you with a isolated Python environment for all your Django projects. The production version didn't work with Snow Leopard yet, so you must install the latest version from the repository. You can do that with:

    sudo easy_install http://bitbucket.org/ianb/virtualenv/get/tip.zip

Virtualenvwrapper will make it easier to switch between environments. Let's install that also:

    sudo easy_install virtualenvwrapper

We will need to add two lines to our `~/.profile` to make it work:

    export WORKON_HOME=$HOME/Sites/virtualenvs
    source /usr/local/bin/virtualenvwrapper_bashrc

Make sure that `WORKON_HOME` is the directory where you will have your Virtual environments. After you have run `source ~/.profile` you can use the package by typing `workon`.

## Imaging (PIL) with JPEG and Freetype2 support

Almost every Django project will make use of the Python Imaging library. To be able to use this with libjpeg and Freetype2 you must have some extra libraries installed. Let's start with libjpeg by going to your `~/Sources` directory and typing the following commands:

    curl -O -L http://downloads.sourceforge.net/project/libjpeg/libjpeg/6b/jpegsrc.v6b.tar.gz
    tar xzvf jpegsrc.v6b.tar.gz
    cd jpeg-6b
    cp /usr/share/libtool/config/config.sub .
    cp /usr/share/libtool/config/config.guess .
    ./configure --enable-shared --enable-static
    make


Before installing you may need to create some extra directories.

    sudo mkdir -p /usr/local/include
    sudo mkdir -p /usr/local/lib
    sudo mkdir -p /usr/local/man/man1

You can now install `libjpeg`.

    sudo make install

Next we need to install the Freetype2 library. This is a lot simpler.

    cd ~/Sources
    curl -O -L http://downloads.sourceforge.net/project/freetype/freetype2/2.3.9/freetype-2.3.9.tar.gz
    tar xvfz freetype-2.3.9.tar.gz
    cd freetype-2.3.9
    ./configure && make && sudo make install clean

## [Combining all the tools](id:combining)

You now have all those great tools installed, but what can you do with them? In this piece I'm going to describe how we work at [Bread & Pepper](http://breadandpepper.com). You will have your own workflow, but maybe you can get some tips from ours. The first thing I needed was a general Django requirements file for PIP. I have all my virtual environments in my `~/Sites/virtualenvs` directory this is also the place for my base requirements file for PIP. Create a new file with `vim ~/Sites/virtualenvs/django-basic-requirements.txt` and fill it with the following:

    # Docutils for admin documentation.
    docutils

    # Python Imaging Library
    http://effbot.org/downloads/Imaging-1.1.6.tar.gz

    # Latest Django version
    -e svn+http://code.djangoproject.com/svn/django/trunk#egg=Django

Save it `:wq`. This file supplies us with the packages we use for every Django project. So every time we start a new project, we only have to supply this file to PIP and the rest is magic.

We now need to create a `virtualenvs` directory and our first Virtual Environment.

	cd ~
	mkdir .virtualenvs
	cd virtualenvs

All our Virtual enviroments will reside in this directory. Let's start building a site called _magicpony_.
  
	cd ~Sites
	mkdir magicpony.com
	mkvirtualenv magicpony --no-site-packages

The above command creates a new virtual environment called `magicpony.com` that is your new isolated Python environment. The `--no-site-packages` argument makes sure the environment is completely isolated by not inheriting the packages in your system wide `$PYTHONPATH`.

Now, let's install our basic Django requirements by supplying the file we created earlier (make sure you're still in the `~/Sites/virtualenvs` directory) to PIP.

    pip install -E magicpony.com -r django-basic-requirements.txt

The `-E magicpony` argument tells PIP to use the magicpony virtual environment.
The `-r django-basic-requirements.txt` argument tells it to look inside a requirements file to know wich packages to install. You can now activate the environment by typing:

    workon magicpony.com

If all goes well, you will see `(magicpony)` placed in front of your prompt. If `workon` doesn't work, this could be because you don't have the right settings for Virtualenvwrapper inside your `~/.profile`.

Let's activate our environment, enter the right directory and create a new Django project:

	workon magicpony.com
	cd ~/Sites/virtualenvs/magicpony.com
	django-admin.py startproject magicpony

Now when we enter the newly created directory, we can go start our Django server with `./manage.py runserver`.

Another thing we do is placing a `requirements.txt` file inside our project directory (also under SCM[^3]) which points to all the specific packages needed for this particular Django project. This way every new developer only has to do the following:

    pip install -E magicpony -r virtualenvs/magicpony.com/magicpony/requirements.txt

And they can start working on the new project.

## Conclusion

Well, it was a long ride, but now you have a great Django development setup. It certainly made our lives easier by not constantly having to make sure that the right packages were installed. We also use the requirements files for our
servers so deployment (with [Fabric](http://docs.fabfile.org 0.9/)) goes easy and secure. If you feel that I left something out, or made same errors, please do contact me.

## Acknowledgements

Thanks to Dan Benjamin from for supplying the instructions on how to install
Mercurial on [Hivelogic] and Rich Atkinson's for the instructions on his
website [JetFar] on how to compile PIL.

[Hivelogic]: http://hivelogic.com/articles/compiling-mercurial-on-snow-leopard/
[Jetfar]: http://jetfar.com/libjpeg-and-python-imaging-pil-on-snow-leopard/
