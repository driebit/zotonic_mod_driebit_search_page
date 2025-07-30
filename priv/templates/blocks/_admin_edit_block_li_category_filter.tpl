{% extends "blocks/_admin_filter.tpl" %}

{% block component %}
    {% include "blocks/_admin_select_textual_component.tpl" blk=blk blocks=blocks name=name %}
{% endblock %}

{% block custom_props %}
    <h3> Categoriën zoekfilters </h3>
    <p> Kies de categoriën die worden getoond in het zoekfilter.</p>
    <div class="checkbox">
        {% for c in m.category.tree_flat %}
            <div>
                <label>
                    <input type="checkbox" name="blocks[].show_categories[]~{{ name }}" {% if c.id|member:blk.show_categories %}checked{% endif %} value="{{ c.id }}" id="{{ c.id }}">
                    {{ c.indent }}{{ c.id.name }} 
                </label>
            </div>
        {% endfor %}
    </div>
{% endblock %}
