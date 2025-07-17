{% if blk.displaymode == 'dropdown' %}
    {% include "search/_dropdown.tpl" names=names filter=filter %}
{% else %}
    {% include "search/_checkbox_fieldset.tpl" label="type" filter=filter names=names %}
{% endif %}