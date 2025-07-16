{% block content %}
    {% if blk.collapse != "uncollapsable" %}
        <details {% if blk.collapse == 'not_collapsed' %}open{% endif %}>
        <summary>{{ blk.filter_title }}</summary>
        {% if blk.displaymode == 'dropdown' %}
            <select name="qcat">
                <option value="">Alles</option>
                {% for cat in blk.show_categories %}
                    {% if cat %}
                        <option value={{ cat }}>{{ m.rsc[cat].title }}</option>
                    {% endif %}
                {% endfor %}
            </select>
        {% else %}
            {% include "search/_checkbox_fieldset.tpl" label="type" name="qcat" names=blk.show_categories %}
        {% endif %}
        </details>
    {% else %}
        <h3> {{ blk.filter_title }} </h3>
        {% if blk.displaymode == 'dropdown' %}
            <select name="qcat">
                <option value="">Alles</option>
                {% for cat in blk.show_categories %}
                    {% if cat %}
                        <option value={{ cat }}>{{ m.rsc[cat].title }}</option>
                    {% endif %}
                {% endfor %}
            </select>
        {% else %}
            {% include "search/_checkbox_fieldset.tpl" label="type" name="qcat" names=blk.show_categories %}
        {% endif %}

    {% endif %}
{% endblock %}