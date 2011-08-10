---
title: Solid FreeBSD Server: the Foundation
description: FreeBSD is my distro of choice whenever I need a server. This guide shows how to build a solid FreeBSD server from scratch.
tags: freebsd, server, guide
keywords: freebsd, server, basic, security, setup, ec2, brightbox
---

![FreeBSD Logo](/images/posts/freebsd-logo.png "FreeBSD Logo")

FreeBSD, the high quality, free, operating system is now available as VPS on [Amazon EC2] (thanks [Colin Percival]) and on [Brightbox]. That should be enough reasons for server enthusiasts to give it a spin. In the following guide I will describe how to build a solid foundation on which to further develop your web- or database-server. I covers things as updating your system, adding administrator users and securing your system. Within no-time you will have a solid, secure server.

[Colin Percival]: http://www.daemonology.net/freebsd-on-ec2/ "EC2 for FreeBSD overview page"
[Brightbox]: http://beta.brightbox.com/beta "Brightbox Cloud beta page"

## Amazon EC2 or Brightbox

You can have a FreeBSD VPS on both Amazon and Brightbox. Below you can find the quick setup instructions for both. If you already have a FreeBSD VPS, you continue to the "[Root Password]" section.

[Root Password]: #root_password

### Amazon EC2

Installing FreeBSD on Amazon's EC2 is now possible on micro instances. It's still in beta, but if you are running a small web application, it comes recommended. You can find the AMI ID's at [Daemonology] website.

To start a new instance, go to [AWS Management Console] and launch a new instance by supplying the AMI ID you selected at [Daemonology]. After the instance has booted, connect to it with the root user and the SSH key you selected on AWS:

    ssh -i amazon_aws.pem root@ec2-compute-1.amazonaws.com

[Daemonology]: http://www.daemonology.net/freebsd-on-ec2/
[AWS Management Console]: https://console.aws.amazon.com/ec2/

### Brightbox

Brightbox also supports FreeBSD servers. After [installing the CLI] you can get a list of the available images with the following command:

    brightbox-images list

This will show us that ``img-aoubd`` is a FreeBSD 8.1 (64 bit) image. The following command will install this image on a nano (512mb) server:

    brightbox-servers create -t typ-4nssg -n "www.wunki.org" img-aoubd

The status of the server is available with the ``brightbox-servers list`` command. You can SSH to the server as the root user if the status is "active".

[installing the CLI]: http://docs.brightbox.com/cli/installation 

## <a id="root_password">Root Password</a>

The first thing you need to do is set the password for root. Type in ``passwd`` and make sure you choose a strong password. You will use this password to run commands as root later on.

## Upgrade to Latest Release

<section class="information">

### What's the diff?

What's the difference between upgrade and update? It turns out that you call
it an upgrade when there is a big version change, often meaning that the first
sequence of the version number gets a plus one. An update is when there is a
small change in the software, mostly a version bump in the final sequence of
the versioning number. So if you grow a beard, don't say your upgrading your
looks, you are just updating them to pirate standards.

</section>

_The latest, stable, release is currently 8.2. You only need to follow these instructions if you have a older version of FreeBSD._ 

My server came with 8.1 installed and a newer release ([8.2]) is available. Let's upgrade to the this release as documented in the [handbook]. The following command analyses the system and looks at what should be upgraded. 

[8.2]: http://www.freebsd.org/releases/8.2R/announce.html

    freebsd-update -r 8.2-RELEASE upgrade

Once this process is completed, you can commit the changes with:

    freebsd-update install

At completion you are asked to reboot. After reboot run the ``freebsd-update`` command again to tie all loose ends.

    shutdown -r now
    freebsd-update install

You're system is now almost up-to-date. We only need to recompile all the packages that are currently installed. For this we first need to get the ports and install a ports management tool.

## Install Packages by Using Ports

<section class="information">

### Ports versus Packages

If you don't want to compile your software through ports, FreeBSD also comes with a package manner similar to [aptitude] found on Debian systems.  In FreeBSD it's called ``pkg_add``. You can read about the benefits on the [Software Installation] page in the FreeBSD handbook. In short, ports for control, packages for convenience.

[aptitude]: http://en.wikipedia.org/wiki/Aptitude_(software)
[Software Installation]: http://www.freebsd.org/doc/en_US.ISO8859-1/books/handbook/ports-overview.html

</section>


Ports are a set of Makefiles, patches and descriptions that will build and install a package optimized for FreeBSD. Run the following two commands as
root:

    portsnap fetch
    portsnap extract

portsnap will download an archive containing all ports and then extract them into the ``/usr/ports`` directory. All ports are categorized in directories, for example "security" and "www".

To manage the ports we are using the [portmaster] management tool.

	cd /usr/ports/ports-mgmt/portmaster
	make install clean

[portmaster]: http://www.freebsd.org/doc/handbook/ports-using.html#PORTMASTER

Your system can't find the portmaster command yet, first run a quick ``rehash`` so the system knows that it exists. You now have two options, if you have updated your system to a new release, it's recommended to build all your packages against the new libraries. Otherwise it suffices to only update the outdated packages.

### Update _all_ packages

With portmaster you can rebuild all ports by typing the following command. Go drink some green thee, coffee will only make you edgy while a more passive stance is currently needed.

	portmaster -af

### Update _outdated_ packages

 If you only want to update the outdated packages, you can first view which packages these are with portmaster:

	portmaster -L

Update the packages with:

	portmaster -a

Your system is now running on the latest release, and updated it to the latest packages.

### Keeping up-to-date

To keep it secure by using the latest packages let's enable a cronjob that daily checks for updates. If there are any, it will e-mail and notify the root user. Edit ``/etc/crontab`` and add the following two lines at the bottom:

	# Daily check for security updates
	@daily                                  root    freebsd-update cron

Currently, system messages are sent to the root user. We want to receive those messages inside our own e-mail mailbox. To change this, alter the following line in ``/etc/aliases``:

    # root:	me@my.domain

to, for example:

    root: petar@example.com

To make the changes active, go into the ``/etc/mail`` directory and run ``make``. From now on, every e-mail send to root will be redirected to your own e-mail address. Run ``mail root`` to send a test e-mail. Fill in the subject, content and send it by pressing ^D.

## Hostname

Hostname is the name given to the **end-point** of your machine. On a machine connected to the internet, the hostname ends with your domain name. But don't forget that a domain name identifies the network, and a hostname the computer. In the shell, execute ``hostname <your hostname>``, this won't set your hostname permanently though.  Alter or add the hostname setting in ``/etc/rc.conf`` to your own, for example:

    hostname="argon.wunki.org"    

Finally, replace ``localhost.my.domain`` in your ``/etc/hosts`` file with the hostname above.

## Unicode

Unicode enables your system to handle non-ASCII characters like "š" and "☃". Add the following two lines to your ``/etc login.conf``, inside the default block at the top. Don't forget to remove the backslash after ``:umask:022``.

    :charset=UTF-8:\
    :lang=en_US.UTF-8:

Run the following command to apply your changes:

    /usr/bin/cap_mkdb /etc/login.conf

After a reboot, run ``locale`` command and you should see that UTF-8 is enabled.

## Setup SSH and Administrator User

For security reasons, we don't want to be running the server as root. Let's create a new user that manages the system by using ``su``. Run the following command to add an user: 

    adduser

When you're asked if you want to invite the user to other groups, invite them to the ``wheel`` group. We want to connect to the server by using our SSH key. Let's add the key to the ``authorized_keys`` file by using ``ssh-copy-id`` command found on most Linux systems and installable on MacOSX through [Homebrew]. Run the following command on your local computer:

    ssh-copy-id <username>@<remote box>

You should now try to login to your server:

    ssh <username>@<remote box>

Next are a few easy changes to SSH that throws fairy dust in the eyes of most script kiddies. If you are logged in with your new user, change to the root account by typing ``su``. The system will then ask for your root password that you changed earlier. Make these changes as the root user, don't logout before you are sure that your new user can still SSH to the server. Edit ``/etc/ssh/sshd_config`` so it contains the following uncommented lines:

    PermitRootLogin no
    PasswordAuthentication no
    AllowUsers <your new username>

It's advised to also change your ``Port`` setting to something different than 22. You have the ports between 1025 and 65536 at your disposal. Save the changes and restart the SSH daemon with ``/etc/rc.d/sshd restart``. Don't forgot to add the new port to security group if you are on Amazon EC2. Before logging out the root user, first try logging in with your new user from your local box:

    ssh -p <port> <username>@<host>

If you are able to connect with this user you can close of the root shell and start using this user account for administrative tasks. You can run commands that require root by switching to the root user with the ``su`` command and supplying the root password.

### SSH Config

Save your memory for something worthwhile and add your servers to the ``.ssh/config`` file. As an example:

    Host example.com
        User demo
        Port 31337

With this entry I'm able to do ``ssh example.com`` and I will connect to "example.com" with user "demo" on port 31337.

## Time synchronization

<section class="information">

### Withstanding the test of time

Want to test if ntp is doing it's work? You can set the time to four thirty by typing ``date 1630`` in your terminal. Reboot the server and run ``date`` to see if your clock is set correctly. If you want to test how ntpd works, don't adjust your time more than 1000 seconds away from the current time because nptd will think your nuts ([sanity check])!

</section>

We want our clock on the server to stay accurate. To accomplish this we will be using [NTP] that connects to the internet and checks if the servers date is still correct. Add the following two lines to your ``/etc/rc.conf`` file:

    ntpdate_enable="YES"
    ntpd_enable="YES"

ntpdate will make sure that your server time is correct when the server boots up. ntpd corrects the clock gradually while your server is running. You can do some further configuration by editing ``/etc/ntp.conf`` but we won't do it here because it's default settings are good for most.

[NTP]: http://www.ntp.org/ "Homepage of the Network Time Protocol"

## Firewall with ipfw

We are going to add a firewall to the server and only keep the SSH port open. You could say that this is unnecessary on EC2, since it has security groups. But the firewall is so easy to setup, it never hurts to have an extra layer of security. ipfw also gives you some possibilities that security groups do not offer. It's installed by default on a FreeBSD box, so we can start by adding the following two lines in ``/etc/rc.conf``:

    firewall_enable="YES"
    firewall_type="open"

This enables the firewall, but also opens up all the ports. This way we don't get locked out after rebooting. Reboot the server and check that you get a list of rules when typing ``ipfw list`` as root. We are going to write our own firewall script that is executed on startup. Don't forget to replace the SSH
port with the port you choose earlier!

Create a file in ``/usr/local/etc/ipfw.rules`` with the following content:

    #!/bin/sh
    IPF="ipfw -q add"

    # clear old rules
    ipfw -q flush

    # loopback
    $IPF 10 allow all from any to any via lo0
    $IPF 20 deny all from any to 127.0.0.0/8
    $IPF 30 deny all from 127.0.0.0/8 to any
    $IPF 40 deny tcp from any to any frag

    # statefull
    $IPF 50 check-state
    $IPF 60 allow tcp from any to any established
    $IPF 70 allow all from any to any out keep-state
    $IPF 80 allow icmp from any to any

    # open port ssh (22000)
    $IPF 110 allow tcp from any to any 22000 in
    $IPF 120 allow tcp from any to any 22000 out

    # deny and log everything
    $IPF 500 deny log all from any to any

Reload these new rules by running ``sh /usr/local/etc/ipfw.rules``. If you did something wrong and you get locked out, just manually reboot your server through the console your host probably supplied you with. All ports are open again on reboot. When all is working, replace the ``firewall_type="open"`` line
in ``/etc/rc.conf`` with the following:

    firewall_script="/usr/local/etc/ipfw.rules"

After a reboot, you should see the correct ports open when running ``ipfw list``.

### Scanning ports

Want to be sure all the ports on your server are closed? Let's scan the server with [nmap]. It's available on all Linux distributions also with [Homebrew] on MacOSX. After installing, do a full scan with ``nmap -v -A <hostname>``.

[nmap]: http://nmap.org/ "The nmap homepage"

## Tools of the trade

An installation of FreeBSD is not complete without some core tools. Following are some that I find necessary. [Vim] text editor that can be found in ``/usr/ports/editors/vim-lite``. 
[tmux], a terminal multiplexer that can be found in ``/usr/ports/sysutils/tmux`` and my shell of choice [ZSH] found
in ``/usr/ports/shells/zsh`` You can install these packages by going to the directory and running ``make install clean``.

[Vim]: http://www.vim.org/
[tmux]: http://tmux.sourceforge.net/
[ZSH]: http://www.zsh.org/

## Solid foundation

That's it, you have a solid foundation for a FreeBSD server. Snapshot it so you can use it in the coming guides where I'll be covering a specialized server.

_Have any comments on how to improve this guide? Please do send me an e-mail. I will incorporate those improvements in the guide._

[Amazon EC2]: http://aws.amazon.com/ec2/ "Amazon EC2 Introduction"
[ports]: http://www.freebsd.org/doc/en_US.ISO8859-1/books/handbook/ports-using.html
[handbook]: http://www.freebsd.org/doc/en/books/handbook/updating-upgrading-freebsdupdate.html
[Homebrew]: http://mxcl.github.com/homebrew/
[sanity check]: http://en.wikipedia.org/wiki/Ntpd#Debugging
