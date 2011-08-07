---
title: Solid FreeBSD Server: Django with Nginx and Gunicorn
description: Running Django on FreeBSD with the help of Nginx and Gunicorn.
tags: freebsd, server, django, nginx, gunicorn
keywords: freebsd, server, django, nginx, gunicorn, supervisor
---

Let's continue in our series of creating, solid, [FreeBSD] server. This time we will get our [Django] web application running on a symbiosis of [gunicorn], [supervisor] and [nginx]. Nginx will run at the frontend and proxy the WSGI requests to gunicorn. The gunicorn processes will be monitored by supervisord, so that if one goes down, they will immediatly be put back up.

_Note: This guide continues where the previous guide "[Solid FreeBSD Server: The Foundation]" ends. If you are creating a new  server, please follow the above guide first. It will show you how to do updates and make your server more secure._

[FreeBSD]: http://www.freebsd.org
[Django]: http://www.djangoproject.com
[gunicorn]: http://gunicorn.org/
[supervisor]: http://supervisord.org/
[nginx]: http://nginx.org/
[Solid FreeBSD Server: The Foundation]: http://www.wunki.org/guides/2011/04/05/solid-freebsd-server-foundation.html

## Virtual environment

By following the "[Solid FreeBSD Server: The Foundation]" guide, we already have an user account set up. Use this account to install your Django applications. These will run in their own virtual enviroment with the help of [virtualenv] and [virtualenvwrapper]. Let's install the packages through the ports system, with the root user:

	cd /usr/ports/devel/py-virtualenv
	make install clean
	# And virtualenvwrapper..
	cd /usr/ports/devel/py-virtualenvwrapper
	make install clean

After installation, make sure you have the following in your shell config file (``~/.bashrc`` if your using Bash, ``~/.zshrc`` if your using ZSH):

	# Virtualenvwrapper
	if [ -e /usr/local/bin/virtualenvwrapper.sh ]; then
  		export WORKON_HOME=$HOME/.virtualenvs
  		source /usr/local/bin/virtualenvwrapper.sh
	fi

Next time you will login with this user, you will see a message that the ``~/.virtualenvs`` directory is created and the scripts are placed there.

[virtualenv]: http://pypi.python.org/pypi/virtualenv
[virtualenvwrapper]: http://www.doughellmann.com/projects/virtualenvwrapper/

We will install our Django project in the ``~/apps`` directory (of the previously created user). Let's start by pulling our django project.

	cd ~/apps
	mkvirtualenv --no-site-packages demoproject
	git clone git@github.com/wunki/demoproject.git

This will install a virtual environment in ``~/.virtualenvs/demoproject`` and clone our project with git into the ``~/apps/demoproject`` directory.

### Dependencies

Next thing is to install the dependencies that are needed to run our django project. Of course, Django itself, but it can easily grow to a big list of Python packages. At [Bread & Pepper] we manage the project dependencies in two [pip] requirements files, a ``requirements-stable.txt`` and a ``requirements.txt``. The ``requirements-stable.txt`` contains all the dependencies, including the version number, that guarantee a running application. For example:
	
	Django==1.3
	docutils==0.7
	django-tagging==0.3.1
	-e hg+https://bitbucket.org/david/django-storages@0db5aa65cc34#egg=storages

And the ``requirements.txt`` file is used for our development enviroment. If a package is running stable there, we can bump up the version in the stable requirements. You can read more about the requirements file format in the [pip documentation]. Test if you can get your Django application running with the ``manage.py`` command.

[Bread & Pepper]: http://www.breadandpepper.com
[pip]: http://www.pip-installer.org/en/latest/index.html
[pip documentation]: http://www.pip-installer.org/en/latest/requirement-format.html

## Gunicorn

Our application can be started up with the ``runserver`` command from Django, but this isn't a production ready server. For that purpose, we are going to install gunicorn in our virtual environment. Make sure that the environment is active and if not, activate it with ``workon demoproject``.

You can now install gunicorn with:

	pip install gunicorn

After installation, you can either run gunicorn with the ``manage.py`` command, or with it's own ``gunicorn`` command. We will use the ``manage.py`` command, and for this to work we need to append ``gunicorn`` to our ``INSTALLED_APPS`` tuple in ``settings.py``. After that, try running your application with ``./manage.py run_gunicorn``. We want to have some additional settings that we will place in ``gunicorn.conf.py`` file inside our project directory. This is my gunicorn configuration:

~~~ {.python}
import os

def numCPUs():
    if not hasattr(os, "sysconf"):
        raise RuntimeError("No sysconf detected.")
    return os.sysconf("SC_NPROCESSORS_ONLN")

user = <your-user>
workers = numCPUs() * 2 + 1
bind = "127.0.0.1:8000"
pidfile = "/tmp/gunicorn-demo.pid"
backlog = 2048
logfile = "home/<your-user>/log/gunicorn_demo.log"
loglevel = "info"
~~~

If that starts up, we want to proxy to this server with our nginx server.

## Nginx

Continue by installing nginx (pronounced "engine-x") proxy. Go to you root account by issuing ``su`` and supplying the root password. Install it through the ports system with the following commands:

	cd /usr/ports/www/nginx
	make install clean

This installs the latest stable version, at the moment of writing, 1.0.4., of nginx. Let the server boot at startup by adding it to your ``/etc/rc.conf``:

	nginx_enable="YES"

You can start the server with the following command:

	/usr/local/etc/rc.d/nginx start

If you have opened up port 80 (HTTP) in your Amazon security group, you should see the message "Welcome to nginx" when you open the domain in your browser.

Let's create a proxy inside nginx that will redirect incoming traffic (port 80) to our local gunicorn (port 8000). Open up ``/usr/local/etc/nginx/nginx.conf`` and replace the ``server`` section with the following:

~~~
server {
  listen       80;
  server_name  www.mydomain.com;
  access_log   /var/log/nginx/nginx-access.log;
		
  location  /media/ {
    root /home/user/apps/demo;
  }
  
  location / {
    proxy_pass   http://127.0.0.1:8000;
  }
}
~~~

This is a minimal configuration that will accomplish the following; when the browser requests the ``/media/`` path, nginx will return static files located in ``/home/user/apps/demo/media/`` directory. Following that is the proxy location which will handle all the remaining requests and proxy them to gunicorn. When you have customized the above config for your own needs, restart the nginx server as root with ``/usr/local/etc/rc.d/nginx restart``. If you don't receive any warnings from nginx, start up your gunicorn process, this time with your user account, and open your domain in the browser. You should now be able to see your Django project. The main things are working right now, we only don't want to manually start our gunicorn processes. For this we need the final piece in the stack, [supervisor].

[supervisor]: http://supervisord.org/ "Supervisor homepage"

### Supervisor

[Supervisor] is a management tool for processes. We will use it to monitor our gunicorn deamons and make sure they stay up. If a process goes down, supervisor will start it again. We can install supervisor through the ports tree:

	cd /usr/ports/sysutils/py-supervisor
	make install clean

[supervisor]: http://supervisord.org/ "Supervisor homepage"

To enable supervisor at startup, we will need to add the following to our ``/etc/rc.conf`` file:

	supervisord_enable="YES"

Next thing is to tell supervisor to spin up our gunicorn and keep watching it. We do this by adding our service to ``/usr/local/etc/supervisord.conf``. Add the following at the bottom:

	[program:demo]
	directory=/home/user/apps/demo
	command=/home/user/.virtualenvs/demo/bin/gunicorn_django -c /home/user/apps/demo/gunicorn.conf.py settings_production.py
	user=user
	umask=022
	autostart=True
	autorestart=True
	redirect_stderr=True

Most important setting of the above is the ``command`` setting. This should point to the ``gunicorn_django`` executable of the virtual enviroment of yourproject, following the gunicorn settings file, finally followed by the django settings file for this server.

## Conclusion

That's all there is to it to get Django deployed on a FreeBSD server. We are currently running our services on the above server and are really happy with the results. Want to speed things up? Look at caching, optimizing queries and using a loadbalancer. For a starting web application, the above will suffice.
