{% extends "blocks/_admin_filter.tpl" %}

{% block component %}
    {% include "blocks/_admin_select_textual_component.tpl" blk=blk blocks=blocks name=name %}
{% endblock %}

{% block custom_props %}
    <h3> Categorie </h3>
    <p> Selecteer een categorie. Alle resources met deze categorie worden zoekfilters. </p>
    <select class="form-control" name="blocks[].selected_category~{{ name }}">
        {% for c in m.category.tree_flat %}
                <option value="{{ c.id }}" {% if c.id == blk.selected_category %}selected="selected"{% endif %}>
                    {{ c.indent }}{{ c.id.title|default:c.id.name }}
                </option>
        {% endfor %}
    </select>
    <br/>

    <h3> Optioneel: predicaat </h3>
    <p> Selecteer een een predicaat waarmee de zoekrestulaten verbonden zijn aan resources met de bovenstaande categorie.
    Als er geen predicaat geselecteerd is kunnen de resources verbonden zijn met elk predicaat  </p>
    <select class="form-control" name="blocks[].selected_predicate~{{ name }}">
        <option value="">Alle predicaten</option>
        {% for name,p in m.predicate %}
                <option value="{{ p.id }}" {% if p.id == blk.selected_predicate %}selected="selected"{% endif %}>
                    {{ p.title }}
                </option>
        {% endfor %}
    </select>
    <br/>
{% endblock %}