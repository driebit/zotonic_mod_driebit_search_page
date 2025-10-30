{% extends "base.tpl" %}

{% block content %}
    {% lib "dist/search_page.css"
           "js/driebit_search_page.js"
     %}

    <div id="searchPage"></div>

    <script type="text/javascript" nonce="{{ m.req.csp_nonce }}">

        const blocks = {{ m.search_filters.json[id] }};

        const language = "{{ z_language|default:'nl' }}";

        const urlParams = new URLSearchParams(window.location.search);
        const queryParams = Array.from(urlParams.entries());
        const textParam = urlParams.get('text');

        const flags = {
            blocks: blocks,
            language: language,
            screenWidth: window.innerWidth,
            queryParams: queryParams
        };

        if (textParam) {
            flags.qs = textParam;
        }

        {% with q.qs as qs %}
        if (!flags.qs) {
            const serverQuery = "{{ qs | escape }}";
            if (serverQuery) {
                flags.qs = serverQuery;
            }
        }
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
            searchApp.ports.updateUrl.subscribe(function(pairs) {
                const params = new URLSearchParams();
                pairs.forEach(function(pair) {
                    if (!pair || typeof pair.key !== 'string') {
                        return;
                    }
                    const value = typeof pair.value === 'string' ? pair.value : '';
                    if (value !== '') {
                        params.append(pair.key, value);
                    }
                });
                const newSearch = params.toString();
                const newUrl = window.location.pathname + (newSearch ? '?' + newSearch : '') + window.location.hash;
                window.history.replaceState({}, '', newUrl);
            });
        }

        cotonic.ready.then(function() {
            cotonic.broker.subscribe("$bridge/origin/status", function() {
                searchApp.ports.connected.send(true);
            });
        });

        window.addEventListener('resize', function() {
            searchApp.ports.screenResized.send(window.innerWidth);
        });
    </script>
{% endblock %}