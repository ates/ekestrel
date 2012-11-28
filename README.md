ekestrel: Erlang client for the Kestrel queue server
====================================================

Configuration
-------------

<pre>
    {ekestrel, [
        {pools, [
            {k1, [
                {size, 5},
                {max_overflow, 3},
                {hostname, "localhost"},
                {port, 22133}
            ]},
            {k2, [
                {size, 5},
                {max_overflow, 3},
                {hostname, "localhost"},
                {port, 22133}
            ]}
        ]}
    ]}
</pre>

Usage
-----

<pre>
    ekestrel:subscribe("queue1").
</pre>
