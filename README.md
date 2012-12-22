fubbit
======

proxying rabbitmq adequately inside an erlang cluster for fun and profit

Usage
---

I have started fubbit to get a handy wrapper around amqp_client.
This far it provides API for connecting to rabbitmq and wraps 
all the tasks you can do with amqp_clent automatically detecting
whether the task should be run asynchronously (amqp\_cast) or syncronously
(amqp\_call).

The main point of that library is replacing records-based API with 
proplist-based API.

Upcoming features: good subscription proxying.

Documentation
---

So far it's undocumented, see what fubbit_connection exports for insight.
But when the wrapper is finished, I'll push some docs. (Hopefully though
after the library is finished, it will be so intuitive in usage that docs
won't be needed ;P)

Deployment
---

Because RabbitMQ deployment is still fucked up and I don't want to use 
rebarized versions cause those are sometimes out of date/hotfix intolerant,
fubbit isn't rebar-compatible. To deploy that library do the following:

```
$ wget https://raw.github.com/manpages/fubbit-deploy/master/get-fubbit.sh
$ wget https://raw.github.com/manpages/fubbit-deploy/master/append.txt
$ cat append.txt >> Makefile
```

Now you have make recipe to install and uninstall fubbit in ``deps`` 
directory. Have fun and consider askind vmware to rebarize their 
stuff. :)