TSVIEW
========

This is a [tshistory][tshistory] plugin which provides a `view` subcommand to the
`tsh` command.

[tshistory]: https://bitbucket.org/pythonian/tshistory

It works like this:

```shell
 $ tsh view postgres://babar:celeste@data_server:5432/
  * Running on http://192.168.56.1:5678/ (Press CTRL+C to quit)
 192.168.56.1 - - [16/Jun/2017 17:23:25] "GET /tsview HTTP/1.1" 200 -
 192.168.56.1 - - [16/Jun/2017 17:23:25] "GET /tsplot? HTTP/1.1" 200 -
 192.168.56.1 - - [16/Jun/2017 17:23:36] "GET /tsplot?series=QHD&series=stocks_at_key_coastal_cies&series=Three_Gorges_Dam_Outflow HTTP/1.1" 200 -
```

At the same time, a browser tab opens, and we can select some series:

![tsh view](https://bitbucket.org/pythonian/tsview/downloads/tsview.png)


As a bonus, we also get a browser for the whole history log, by a
slight modification of the url, from `http://192.168.56.1:5678/tsview`
to `http://192.168.56.1:5678/tsviewlog`:

![tsh viewlog](https://bitbucket.org/pythonian/tsview/downloads/tsviewlog.png)

