{% extends "base.tpl" %}

{% block content %}
    {% lib "dist/search_page.css"
           "js/driebit_search_page.js"
     %}
    
    <div id="searchPage"></div>

    <script type="text/javascript" nonce="{{ m.req.csp_nonce }}">

        const blocks = {{ m.search_filters.json[id] }};

        const language = "{{ z_language|default:'nl' }}";

        const flags = 
            { blocks: blocks,
              language: language,
              screenWidth: window.innerWidth
            };


        const searchApp = Elm.SearchPage.init({flags: flags, node: document.getElementById('searchPage')});

        searchApp.ports.searchPageCall.subscribe(function(call) {
            cotonic.broker.call(call.topic, call.parameters, {timeout: 5000})
                .then(function(reply) {
                    searchApp.ports.searchPageReply.send({topic: call.replyTopic, reply: reply, });
                    console.log("Reply from " + call.topic, reply);
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

        window.addEventListener('resize', function() {
            searchApp.ports.screenResized.send(window.innerWidth);
        });
    </script>
{% endblock %}