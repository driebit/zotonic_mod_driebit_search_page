{% block content %}
    {% if blk.collapse != "uncollapsable" %}
        <details {% if blk.collapse == 'not_collapsed' %}open{% endif %}>
        <summary>{{ blk.filter_title }}</summary>
            {% include "search/_display_filter.tpl" blk=blk filter="qcat" %}
        </details>
    {% else %}
        <h3> {{ blk.filter_title }} </h3>
        {% include "search/_display_filter.tpl" blk=blk filter="qcat" %}
    {% endif %}
{% endblock %}