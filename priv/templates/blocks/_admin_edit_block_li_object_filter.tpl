{% extends "blocks/_admin_filter.tpl" %}

{% block component %}
    {% include "blocks/_admin_select_textual_component.tpl" blk=blk blocks=blocks name=name %}
{% endblock %}

{% block custom_props %}
    <h3>{_ Category _}</h3>
    <p>{_ Select a category. All resources with this category become search filters. _}</p>
    <select class="form-control" name="blocks[].selected_category~{{ name }}">
        {% for c in m.category.tree_flat %}
            <option value="{{ c.id }}" {% if c.id == blk.selected_category %}selected="selected"{% endif %}>
                {{ c.indent }}{{ c.id.title|default:c.id.name }}
            </option>
        {% endfor %}
    </select>
    <br/>

    <h3>{_ Optional: predicate _}</h3>
    <p>{_ Select a predicate that links the search results to resources with the above category.
    If no predicate is selected, resources can be linked with any predicate. _}</p>
    <select class="form-control" name="blocks[].selected_predicate~{{ name }}">
        <option value="">{_ All predicates _}</option>
        {% for name,p in m.predicate %}
            <option value="{{ p.id }}" {% if p.id == blk.selected_predicate %}selected="selected"{% endif %}>
                {{ p.title }}
            </option>
        {% endfor %}
    </select>
    <br/>
{% endblock %}