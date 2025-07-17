{% block content %}
    {% with m.search[{query cat=blk.selected_category  pagelen=100}]|make_list as names %}
        {# TODO: add support for predicate #}
        {% with `qhasobject` as filter %}
            {% if blk.collapse != "uncollapsable" %}
                <details {% if blk.collapse == 'not_collapsed' %}open{% endif %}>
                <summary>{{ blk.filter_title }}</summary>
                    {% include "search/_display_filter.tpl" names=names filter="qhasobject" %}
                </details>
            {% else %}
                <h3> {{ blk.filter_title }} </h3>
                {% include "search/_display_filter.tpl" names=names filter="qhasobject" %}
            {% endif %}
        {% endwith %}
    {% endwith %}
{% endblock %}