{% extends "base.tpl" %}

{% block content %}
    {% lib "dist/search_page.css"
           "js/driebit_search_page.js"
     %}

     

    
    <div id="searchPage"></div>

    <script type="text/javascript" nonce="{{ m.req.csp_nonce }}">

        const blocks = {{ m.search_filters.json[id] }};

        const language = "{{ z_language|default:'nl' }}";

        {% with q.qs as qs %}
            const flags = 
                { blocks: blocks,
                language: language,
                screenWidth: window.innerWidth
                {% if qs %},qs: "{{qs | escape }}"{% endif %}
                };
        {% endwith %}

        const searchApp = Elm.SearchPage.init({flags: flags, node: document.getElementById('searchPage')});

        searchApp.ports.searchPageCall.subscribe(function(call) {
            cotonic.broker.call(call.topic, call.parameters, {timeout: 5000})
                .then(function(reply) {
                    searchApp.ports.searchPageReply.send({topic: call.replyTopic, reply: reply});
                })
                .catch(function(e) {
                    console.log("Error on call to " + call.topic, e);
                });
            });

        if (searchApp.ports.updateUrl) {
            searchApp.ports.updateUrl.subscribe(function(params) {
                const url = new URL(window.location.href);
                url.search = "";

                params.forEach(function(param) {
                    if (param && param.key && param.value.length > 0) {
                        url.searchParams.set(param.key, param.value);
                    }
                });

                window.history.replaceState({}, "", url.toString());
            });
        }

        cotonic.ready.then(
            function() {
                cotonic.broker.subscribe("$bridge/origin/status", function(msg){
                    searchApp.ports.connected.send(true);
                })
            });

        window.addEventListener('resize', function() {
            searchApp.ports.screenResized.send(window.innerWidth);
        });
    </script>
{% endblock %}
