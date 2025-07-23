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
                    <input type="radio" name="blocks[].displaymode~{{ name }}" {% if blk.displaymode == 'buttons' %}checked{% endif %} value="buttons" id="buttons">
                    {_ Knoppen met tijdsperiodes _}
                </label>
            </div>
            <div>
                <label>
                    <input type="radio" name="blocks[].displaymode~{{ name }}" {% if blk.displaymode == 'calendar' %}checked{% endif %} value="calendar" id="calendar">
                    {_ Kalender (één datum selecteerbaar) _}
                </label>
            </div>
        </div>
    </div>


    <h3>Type datum</h3> 
        <div class="controls">
        <div class="radio">
            <div>
                <label>
                    <input type="radio" name="blocks[].date_prop~{{ name }}" {% if blk.date_prop == 'buttons' %}checked{% endif %} value="publication_date" id="publication_date">
                    {_ Publicatiedatum _}
                </label>
            </div>
            <div>
                <label>
                    <input type="radio" name="blocks[].date_prop~{{ name }}" {% if blk.date_prop == 'calendar' %}checked{% endif %} value="date" id="date">
                    {_ Event datum _}
                </label>
            </div>
        </div>
    </div>
{% endblock %}