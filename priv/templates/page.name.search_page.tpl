{% extends "base.tpl" %}

{% block content %}
    {% lib "dist/search_page.css"
           "js/driebit_search_page.js"
     %}
    
    <div id="multiselect"></div>

    <script type="text/javascript" nonce="{{ m.req.csp_nonce }}">

        const blocks = {{ m.search_filters.json[id] }};


        console.log(blocks);


        const searchApp = Elm.SearchPage.init({flags: blocks, node: document.getElementById('multiselect')});

        searchApp.ports.searchPageCall.subscribe(function(call) {
            console.log(call);
            {# if (call.replyTopic === "SearchReply") {
                const urlParams = new URLSearchParams(call.parameters).toString();
                const url = new URL(location);
                // combine the current URL with the new parameters
                url.search = urlParams;
                history.pushState({}, "", url);
            } #}
            cotonic.broker.call(call.topic, call.parameters, {timeout: 5000})
                .then(function(reply) {
                    console.log(reply);
                    searchApp.ports.searchPageReply.send({topic: call.replyTopic, reply: reply, });
                })
                .catch(function(e) {
                    console.log("Error on call to " + call.topic, e);
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