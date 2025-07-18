{% if blk.displaymode == 'dropdown' %}
    {% include "search/_dropdown.tpl" names=names filter=filter %}
{% elif blk.displaymode == 'multiselect' %}
    Hallo daar!
    <div id="multiselect"></div>
{% else %}
    {% include "search/_checkbox_fieldset.tpl" label="type" filter=filter names=names %}
{% endif %}