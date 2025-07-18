{% extends "admin_edit_widget_i18n.tpl" %}

{% block widget_title %}
    {_ Filter Categorie _}<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-block-{{ #block }}{% endblock %}

{% block widget_content %}
{% endblock %}

{% block widget_content_nolang %}

    <h3>Instellingen</h3>
    <div class="form-group row">
        <label class="control-label col-md-3" for="filter_title">Titel</label>
        <div class="col-md-9">
            <input type="text" name="blocks[].filter_title~{{ name }}" value="{{ blk.filter_title }}" id="filter_title" class="form-control" />
        </div>
    </div>
    <div class="controls">
        <div class="radio">
            <div>
                <label>
                    <input type="radio" name="blocks[].collapse~{{ name }}" {% if blk.collapse == 'collapsed' %}checked{% endif %} value="collapsed" id="collapsed">
                    {_ Toon ingeklapt _}
                </label>
            </div>
            <div>
                <label>
                    <input type="radio" name="blocks[].collapse~{{ name }}" {% if blk.collapse == 'not_collapsed' %}checked{% endif %} value="not_collapsed" id="not_collapsed">
                    {_ Toon uitgeklapt _}
                </label>
            </div>
            <div>
                <label>
                    <input type="radio" name="blocks[].collapse~{{ name }}" {% if blk.collapse == 'uncollapsable' %}checked{% endif %} value="uncollapsable" id="uncollapsable">
                    {_ Toon zonder uitklapper _}
                </label>
            </div>
        </div>
    </div>


    <h3>Display modus</h3> 
        <div class="controls">
        <div class="radio">
            <div>
                <label>
                    <input type="radio" name="blocks[].displaymode~{{ name }}" {% if blk.displaymode == 'checkboxes' %}checked{% endif %} value="checkboxes" id="checkboxes">
                    {_ Checkboxes (meerdere opties selecteerbaar) _}
                </label>
            </div>
            <div>
                <label>
                    <input type="radio" name="blocks[].displaymode~{{ name }}" {% if blk.displaymode == 'dropdown' %}checked{% endif %} value="dropdown" id="dropdown">
                    {_ Dropdown (één optie selecteerbaar) _}
                </label>
            </div>
            <div>
                <label>
                    <input type="radio" name="blocks[].displaymode~{{ name }}" {% if blk.displaymode == 'multiselect' %}checked{% endif %} value="multiselect" id="dropdown">
                    {_ Multiselect met zoek _}
                </label>
            </div>
        </div>
    </div>


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