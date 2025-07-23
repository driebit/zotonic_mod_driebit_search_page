{% extends "base.tpl" %}

{% block content %}
    {% lib "dist/search_page.css"
           "js/driebit_search_page.js"
     %}
    
    <div id="multiselect"></div>

    <script type="text/javascript" nonce="{{ m.req.csp_nonce }}">

        const blocks = 
            [{% for block in m.rsc[`search_page`].blocks %}
                {{ block|fjson }},
            {% endfor %}
            ]
        ;

        console.log(blocks);


        const searchApp = Elm.SearchPage.init({flags: blocks, node: document.getElementById('multiselect')});

        searchApp.ports.searchPageCall.subscribe(function(call) {
            console.log(call);
            cotonic.broker.call("bridge/origin/model/search/get", call, {timeout: 5000})
                .then(function(reply) {
                    console.log(reply);
                    searchApp.ports.searchPageReply.send({topic: "reply/payload/result/result", reply: reply});
                })
                .catch(function(e) {
                    console.log("Error on call to " + "bridge/origin/model/search/get", e);
                });
            });

        cotonic.ready.then(
            function() {
                // add extra timeout for subscribing to a topic
                setTimeout(function() { 
                    searchApp.ports.connected.send(true);
                }, 100);
            });
    </script>
{% endblock %}