---
title: Solid FreeBSD Server: PostgreSQL 9.0
description: Solid FreeBSD Server: PostgreSQL 9.0 on FreeBSD.
tags: freebsd, server, postgresql
---
You want your [PostgreSQL] database on the best platform available? Let me help you out with that. This guide continues where "[Solid FreeBSD Server: the Foundation]" guide ends. It shows you how to install a PostgreSQL 9.0.3 server which can be connected to from other hosts through a SSL secured connection.

[Solid FreeBSD Server: the Foundation]: /guides/2011/04/05/solid-freebsd-server-foundation.html

## Installation

We will continue from the previous guide and create a secure [PostgreSQL] server which can be accessed remotely. Let's start by installing:

    cd /usr/ports/databases/postgresql90-server
    make install clean

The only change from the default configuration I made was by enabling the optimized CFLAGS option. Enable the use of PostgreSQL by adding this lines to your ``/etc/rc.conf``:

     postgresql_enable="YES"

Following we will initialize the database by the following command:

    /usr/local/etc/rc.d/postgresql initdb

Let's start up the server:

    /usr/local/etc/rc.d/postgresql start

We now have a working PostgreSQL server. Let's create a database to check if everything went well. To do this, we must first switch to the ``pgsql`` user that administrates PostgreSQL. First we will add a PostgreSQL user that has access to the database.

    su pgsql
    createuser -P pg_test

Say no when asked if you want to make the user a superuser, allow to create databases or allow to create new roles. Next we will create a Unicode database that this user has access to:

    /usr/local/bin/createdb db_test -O pg_test --encoding=UNICODE

If you don't get any messages, everything went well. Don't delete the database just yet, we are going to use this one to test remote connectivity which we are going to set up next.

[PostgreSQL]: http://www.postgresql.org/

## Remote connectivity

Remote connectivity enables you to connect to the server from other machines. Currently, only local connections are accepted.

### Why ain't it working?

For debugging purposes, you can enable the output that is given to us by PostgreSQL. Alter the following line in
``/usr/local/pgsql/data/postgresql.conf``:

    silent_mode = off

Restart the server with ``/usr/local/etc/rc.d/postgresql restart`` for the changes to take effect.

We want the PostgreSQL server to listen to it's own public IP address instead of only local connections. Again in ``/usr/local/pgsql/data/postgresql.conf`` append your remote address to the following line:

    listen_addresses = 'localhost, 123.45.67.89'

Restart the PostgreSQL to apply this setting. _You can find your remote address by looking at the output of ``ifconfig`` command._

Next we will change the [authentication method] for remote users by editing ``/usr/local/pgqsl/data/pg_hba.conf``. The following settings are the default:

    # "local" is for Unix domain socket connections only
    local   all             all                                     trust
    # IPv4 local connections:
    host    all             all             127.0.0.1/32            trust
    # IPv6 local connections:
    host    all             all             ::1/128                 trust

[authentication method]: http://www.postgresql.org/docs/9.0/static/auth-methods.html

Remote access can be enabled by altering the IPv4 setting. The current record allows connection to all databases, for all users, for local connections using the "trust" authentication method. Trust meaning that all users that can connect to the server are also allowed to connect to the database. We want to enable users to connect with a username and password to the database. Therefore we will change the authentication for IPv4 to the following:

    host    all             all             0.0.0.0/0               md5

This way, everyone can connect to the server if they supply a correct user and password. Restart the server for the change to have any effect. Before trying to connect to the server, we must open up the port in our firewall. Add the following two rules to your firewall script in ``/usr/local/etc/ipfw.rules``:

    # open port for postgresql (5432)
    $IPF 130 allow tcp from any to any 5432 in
    $IPF 140 allow tcp from any to any 5432 out
    
Apply the settings by running ``sh /usr/local/etc/ipwf.rules``. You can now try to connect to the remote database by typing on your local box:

    psql -U pg_test -d db_test -h 192.168.99.67

Replace "192.168.99.67" by the IP address of you server. If you followed along with the above, you should be able to connect to the server. If you can't connect, but also don't get any errors, check your firewall rules.

### NB: psql Not Found

psql command comes with the PostgreSQL installation package. I didn't have it on my Mac, so I installed PostgreSQL with the help of [Homebrew].

    brew install postgresql

You now have a PostgreSQL that is configured for remote access, but the current configuration is not secure enough. Everyone is able to connect to the server and all the communication that is send is unencrypted. In the following chapter we will make it more secure by using SSL to encrypt and also limit those that can connect to PostgreSQL.

[Homebrew]: http://mxcl.github.com/homebrew/

## Security measures

We want our communication with PostgreSQL to be encrypted. PostgreSQL uses the [OpenSSL] package to accomplish this. Let's start by installing this from ``/usr/ports/security/openssl``. The default configuration will do. As root, run the following commands:

    cd /usr/ports/security/openssl
    make install clean

[OpenSSL]: http://www.openssl.org/

Next, copy the default OpenSSL configuration file:

    cp /usr/local/openssl/openssl.cnf.sample /usr/local/openssl/openssl.cnf

The default settings will do fine. We need a certificate that will be used for the encryption. Create the certificate request with the following commands:

    openssl req -new -text -out /usr/local/pgsql/data/server.req

Remember your PEM pass phrase and when asked for a common name, enter the domain of your server. For example "postgresql.example.com". Finally, leave the challenge password empty.

Next we need to remove PEM pass phrase or we will be asked the password at every boot. Remove the password with the following command:

    openssl rsa -in privkey.pem -out server.key
    rm privkey.pem

Enter the old passphrase to unlock the key. Next enter the following command to create the certificate with the correct rights:

    openssl req -x509 -in server.req -text -key server.key -out server.crt
    chmod og-rwx server.key
    chown pgsql:pgsql server.key server.crt server.req

Next we will tell PostgreSQL to only use SSL connection from remote servers. Edit ``/usr/local/pgsql/pg_hba.conf`` and alter the IPv4 line as follows:

    hostssl all             all             0.0.0.0/0               md5

And finally enable SSL by uncommenting and changing the following line in
``/usr/local/pgsql/postgresql.conf``:

    ssl = on

Restart PostgreSQL and try to connect with your local box. If all went well, you should see something as the following:

    Password for user pg_test: 
    psql (9.0.3)
    SSL connection (cipher: DHE-RSA-AES256-SHA, bits: 256)
    Type "help" for help.

    db_test=> 

In a measure of security by obscurity, we will change the default PostgreSQL port to a new one. Again, you have the ports 1025 through 65536 at your disposal. Change the port by change "port" in
``/usr/local/pgsql/postgresql.conf``. For example:

    port = 2345

Don't forgot to reflect the changes in your firewall rules
``/usr/local/etc/ipfw.rules``.:

    # open port for postgresql (2345)
    $IPF 130 allow tcp from any to any 2345 in
    $IPF 140 allow tcp from any to any 2345 out

Apply the new firewall rules and restart PostgreSQL for the changes to take effect. Don't forget to use the ``-p`` flag, pointing to your port when using
the ``psql`` command.

### Three steps to create a new database

First thing you have to do is initialize your PostgreSQL database with the following command:

	/usr/local/etc/rc.d/postgresql initdb

This will setup all the directories that are needed by PostgreSQL. Now you need a database itself, but before that, we will setup a new user. I always create a user per database. This way, if a user get's compromised, they don't have access to all your databases.

	su pgsql
	createuser -P pg_new_user

Say no when asked if you want to make the user a superuser, no to allow to create databases or allow to create new roles. Next we will create a Unicode database that this user has access to:

    /usr/local/bin/createdb db_new_db -O pg_new_user --encoding=UNICODE

## Conclusion

That's it, you now have a "db_new_db" database, which you can connect to with the "pg_new_user" user. This is also the end of this guide. There is now a whole lot of cool things you can do with your new database. Look into some [optimization] options or [streaming replication].

[optimization]: http://robots.thoughtbot.com/post/2638538135/postgresql-performance-considerations "PostgreSQL performance options"
[streaming replication]: http://wiki.postgresql.org/wiki/Streaming_Replication "Streaming replication guide"

