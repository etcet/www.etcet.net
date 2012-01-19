--
title: Centralized Logging with Sentry
keywords: django, sentry, freebsd
tags: django
description: Setting up Django's sentry
--

I like getting email, being the `Exception` if it starts with "[Django] Error…". I'm willing to bet that 95% of the Django projects out there use email for error handling their production sites. At [Bread & Pepper] we decided to do something about it because we were losing errors in our inbox and lost track of what was fixed.

[Bread & Pepper]: http://breadandpepper.com/

Luckily, there is a great toolkit called [Sentry] from the people of [Disqus] which let's you do centralized logging. Any kind of logging, not just errors. Let me try to sell it to you by listing some features from it [introduction post]:

[Sentry]: http://readthedocs.org/docs/sentry/en/latest/
[Disqus]: http://disqus.com/
[introduction post]: http://blog.disqus.com/post/1178923988/django-sentry?bf586bc0

1. Intelligently grouping error messages.
2. Identifying erring servers, functions, and URLs.
3. Streaming errors to the list as they happen without a manual refresh.

In the following post I will show you how to setup a logging server and also how to test it in your local Django project. My server of choice is running FreeBSD, but you can use the guide here for any *Nix system.

## Installing Sentry

First, get yourself a server. I got myself a VPS at [TransIP] but you can get one at [Linode], [AWS] or [Brightbox]. Do the basic configuration like setting up users and securing SSH access. If you decided to run FreeBSD -- good choice -- just follow my "[Solid FreeBSD Server: the Foundation]" guide.

PostgreSQL will be our database. I'm not going to guide you through the installation of it because others have already done that for me. Here are some guides for [Ubuntu], [Debian] and [FreeBSD]:

Debian

:   [Use PostgreSQL Relational Databases on on Debian 6]

Ubuntu

:   [Use PostgreSQL Relational Databases on Ubuntu 10.10]

FreeBSD
:   [Solid FreeBSD Server: PostgreSQL 9.0] (my own guide)

[TransIP]: www.transip.nl
[Linode]: www.linode.com
[AWS]: http://aws.amazon.com
[Brightbox]: http://brightbox.com/
[Solid FreeBSD Server: the Foundation]: /posts/2011-04-05-solid-freebsd-server-foundation.html
[Ubuntu]: http://www.ubuntu.com/
[Debian]: http://www.debian.org/
[FreeBSD]: http://www.freebsd.org/
[Use PostgreSQL Relational Databases on on Debian 6]: http://library.linode.com/databases/postgresql/debian-6-squeeze
[Use PostgreSQL Relational Databases on Ubuntu 10.10]: http://library.linode.com/databases/postgresql/ubuntu-10.10-maverick
[Solid FreeBSD Server: PostgreSQL 9.0]: /posts/2011-05-07-solid-freebsd-server-postgresql-90.html

PostgreSQL installed? Good! Let's create the database for Sentry:
	
	# Switch to pgsql user
	# On Debian/Ubuntu this is postgres
	sudo su - pgsql
	
	# Create a user, say 'n' to all role questions
	createuser -P pg_sentry

	# Create the database
	createdb db_sentry -O pg_sentry --encoding=UNICODE

We will install [Sentry] inside a virtual environment by using [virtualenv]. To install this we will be using our Python Package manager of choice: [pip]. 

Install it on FreeBSD by going to `/usr/ports/devel/py-pip` and run `make install clean` as root. For other systems, read the [pip installation instructions]. Pip makes it easy to install "virtualenv" and it's helper "virtualenvwrapper". Run the following as root:

	pip install virtualenv
	pip install virtualenvwrapper

[pip]: www.pip-installer.org/
[pip installation instructions]: www.pip-installer.org/en/latest/installing.html
[virtualenv]: www.virtualenv.org/en/latest/index.html

Virtualenvwrapper requires some environment settings to work correctly. I have the following in my `.zshrc` to have all my virtual environments in the `~/.virtualenvs` directory. If you have Bash, add it to your `.bashrc`.

	export WORKON_HOME=$HOME/.virtualenvs
	
	# Check if virtualenvwrapper is installed, if so, run it!
	# As you can see, the location of my virtualenvwrapper is in
	# `/usr/local/bin/` but yours could be in `/usr/bin/`
	if [ -e "/usr/local/bin/virtualenvwrapper.sh" ]; then
       source /usr/local/bin/virtualenvwrapper.sh
	fi

Logout/login for the changes to take effect. If all went well, you should get's some messages saying that scripts have been installed in ``.virtualenvs`` directory. All is set for creating the virtual environment `sentry`:

	mkdir ~/sentry
	mkvirtualenv sentry

Your virtual environment will be activated when you call the `mkvirtualenv` script, but remember to activate it with `workon sentry` the next time you want to work on the sentry project. All our bases are covered now, time to actually start installing Sentry. I love working with stable requirements and pip, so let's create a `requirements.txt` file inside our `sentry` directory and fill it with the requirements for Sentry.

	touch ~/sentry/requirements.txt
	echo "Django==1.3.1" >> ~/sentry/requirements.txt
	echo "sentry==2.0.0-RC6" >> ~/sentry/requirements.txt
	echo "psycopg2==2.4.2" >> ~/sentry/requirements.txt

_You don't have to add Django in your requirements file because Sentry takes care of all the dependencies. But we want to use the latest stable version of Django, that's why we put it in there._

Now, install it with a simple `pip install -r ~/sentry/requirements.txt`. A lot of libraries will get installed, but at the end you should see 

> Successfully installed Django sentry django-paging django-indexer django-templatetag-sugar raven python-daemon eventlet South kombu django-kombu simplejson lockfile greenlet anyjson amqplib psycopg2
> Cleaning up…"

We need to initialize a base configuration for Sentry. The following command will create one for you in the `sentry` directory:

	sentry init ~/sentry/sentry.conf.py

Edit the configuration, I ended up with the following configuration:

~~~ {.python .numberLines}
import os.path

from sentry.conf.server import *

ROOT = os.path.dirname(__file__)

DATABASES = {
    'default': {
        # We use the PostgreSQL database we created earlier
        'ENGINE': "django.db.backends.postgresql_psycopg2",
        'NAME': "db_sentry",
        'USER': 'pg_sentry',
        'PASSWORD': 'your_password',
        'HOST': 'localhost',
    }
}

SENTRY_KEY = "SOME_KEY"

# Set this to false to require authentication
SENTRY_PUBLIC = True

SENTRY_WEB_HOST = '0.0.0.0'
SENTRY_WEB_PORT = 9000
~~~

Note that we have kept the `SENTRY_PUBLIC` setting to `True`. That's so we can easily test it later on, we will switch that back to `False` when going live. Rest of the settings speak for themselves. Before trying to start Sentry, we need to initialize it's database with:
	
	cd ~/sentry
	sentry upgrade --config=sentry.conf.py

We can start the server now with the following command:

	cd ~/sentry
	sentry start --config=sentry.conf.py

When your port is open, you can now visit your web server at port 9000, for example `http://example.com:9000`

## Nginx

It's inconvenient to visit the server on port "9000" and there is a powerful server which is great at serving static media. Let's install Nginx and proxy it to Sentry. On FreeBSD we can install Nginx by going to `/usr/ports/www/nginx` and running `make install clean` as root. Add `nginx_enable="YES"` to `/etc/rc.conf` so the Nginx starts at boot. Start the server right away by running the following command as root:

	 /usr/local/etc/rc.d/nginx start

Nginx is Ubuntu or Debian is also easy to install, just follow one of the guides in [Linode's Library]. Following is the config I'm using to setup Nginx, if you replace the `example` parts with the your own settings, all should work. Be aware though, that Ubuntu/Debian work with a `sites_enabled` directory which they all include in the main config. If you have that, you only have to paste the `server { … }` part inside a file. For example `/etc/nginx/sites-enabled/sentry.conf`.

[Linode's Library]: http://library.linode.com/web-servers/nginx/installation

~~~ {.conf}
# On Ubuntu/Debian this is usually www-data
# On FreeBSD www
user  www www;

# Worker processes, rule, 1 per CPU
worker_processes  2;

error_log  /var/log/nginx-error.log;

# Some optimizations
worker_rlimit_nofile 8192;
events {
    worker_connections  8000;
}

http {
    include       mime.types;
    default_type  application/octet-stream;

    log_format  main  '$remote_addr - $remote_user [$time_local] "$request" '
                      '$status $body_bytes_sent "$http_referer" '
                      '"$http_user_agent" "$http_x_forwarded_for"';

    access_log  /var/log/nginx-access.log  main;
    
    sendfile       on;
    tcp_nopush     on;
    tcp_nodelay    off;
    keepalive_timeout  20;

    # Optimize file access
    open_file_cache max=1000 inactive=20s;
    open_file_cache_valid 30s;
    open_file_cache_min_uses 2;
      
    # Gzipping
    gzip              on;
    gzip_http_version 1.0;
    gzip_comp_level   5;
    gzip_proxied      any;
    gzip_min_length   512;
    gzip_buffers      4 8k;
    gzip_vary         on;
    gzip_types
      text/css
      text/javascript
      text/xml
      text/plain
      text/x-component
      application/javascript
      application/x-javascript
      application/json
      application/xml
      application/rss+xml
      font/truetype
      font/opentype
      application/vnd.ms-fontobject
      image/svg+xml;
    
    # Some version of IE 6 don't handle compression well on some mime-types, so
    # just disable for them
    gzip_disable        "MSIE [1-6]\.";

    server {
        listen 80;

        # Replace with your own hostname
        server_name sentry.example.com;

        access_log /home/example/sentry/logs/nginx-access.log;
        
        # Set the charset
        charset utf-8;

        # Let Nginx handle the static data. Make sure you direct the static
        # path to the correct location
        location /_static {
            alias /home/example/.virtualenvs/sentry/lib/python2.7/site-packages/sentry/static/;
            expires 14d;
            access_log off;
        }
        
        # Proxy to Sentry
        location / {
            proxy_pass         http://localhost:9000;
            proxy_redirect     off;

            proxy_set_header   Host             $host;
            proxy_set_header   X-Real-IP        $remote_addr;
            proxy_set_header   X-Forwarded-For  $proxy_add_x_forwarded_for;
        }
    }
}
~~~

Restart Nginx and go back to your Sentry directory. Run Sentry again as follows:

	cd ~/sentry
	sentry start --config=sentry.conf.py

When you now visit the server in your browser, you should see Sentry without having to supply the `:9000` port number. But you don't want to have to run Sentry manually every time, so let's finish it up by installing "Supervisor" to monitor the process.

## Supervisor

Installing supervisor on FreeBSD can be done by going to `/usr/ports/sysutils/py-supervisor` and running `make install clean` as root. Make sure it starts at boot by adding the following to `/etc/rc.conf`

	supervisord_enable="YES"

Start it right now by running this as root:

	/usr/local/etc/rc.d/supervisord start

If you are running on something else than FreeBSD, just follow the [Supervisor Installation] documentation.

[Supervisor Installation]: http://supervisord.org/installing.html

To monitor the Sentry process by Supervisor, add the following to `supervisord.conf`. On FreeBSD it's located in `/usr/local/etc/supervisord.conf`. Again replace example with the user your running Sentry with:

	[program:sentry]
	directory=/home/example/sentry
	command=/home/example/.virtualenvs/sentry/bin/sentry start --config=/home/example/sentry/sentry.conf.py
	user=example
	umask=022
	autostart=True
	autorestart=True
	redirect_stderr=True

To let supervisor find this new section we must first reload it's configuration by running the following as root `supervisorctl reload`. Check if everything went as it should by restarting your VPS. Let's assume everything went well, and if not, that you are a very good debugger. Final step is to secure Sentry by using authentication. Add a superuser by running the following:

	workon sentry
	cd ~/sentry
	sentry manage createsuperuser --config=sentry.conf.py
	
Edit your `sentry.conf.py` file and set:

	SENTRY_PUBLIC = False

Restart Sentry to activate the new configuration by running `supervisorctl restart sentry` as root. Go to your Sentry website and you are done!

## Install in your Django project

Connecting your Django project to Sentry requires a Sentry client. For Python such a client is [Raven]. Find a Django project or create a new one which you want to use Raven for. First, install raven with the command `pip install raven --upgrade` and open the `settings.py` of your Django project.

Then follow along with the steps in the Raven's [Configuring Django] documentation. The `SENTRY_KEY` can be found on the server in your `sentry.conf.py` and `SENTRY_SERVERS` is the name of your server.

[Raven]: http://raven.readthedocs.org/en/latest/index.html
[Configuring Django]: http://raven.readthedocs.org/en/latest/config/django.html

If you have setup `LOGGING` as told in the configuration setup you can test if it works by adding the following to one of your views and visiting it in your browser. You should see the error without refreshing at your Sentry server.

~~~ {.python}
import logging

# Get an instance of a logger
logger = logging.getLogger(__name__)
logger.error('There was some crazy error',
             exc_info=True,
             extra={'request': request,})

~~~

## Get Logging

Your centralized logging setup is working. Now it's time to explain how it works to your co-workers! Also a good thing to read is Django's [logging documentation] before going through your project to find some important logging points. If you missed something in this guide, let me know by means of tweeting or mailing.

[logging documentation]: https://docs.djangoproject.com/en/dev/topics/logging/


