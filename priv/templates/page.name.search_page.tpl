{% extends "base.tpl" %}

{% block content %}
    {% lib "dist/search_page.css" %}

    <div id="live"></div>

    {% live topic="model/location/event/qlist"
            template="search_form.tpl"
            target="live"
            method="patch"
            is_new_query
    %}
{% endblock %}