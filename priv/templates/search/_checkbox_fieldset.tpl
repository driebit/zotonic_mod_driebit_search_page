<fieldset class="c-fieldset">
    {% for n in names %}
        {% if n %}
            {% with m.rsc[n] as rsc %}
                {% include "search/_checkbox.tpl" name=name value=rsc.id label=rsc.title %}
            {% endwith %}
        {% endif %}
    {% endfor %}
</fieldset>