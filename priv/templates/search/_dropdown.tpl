<select name="{{ filter }}">
    <option value="">Alles</option>
    {% for name in names %}
        {% if name %}
            <option value={{ name }}>{{ m.rsc[name].title }}</option>
        {% endif %}
    {% endfor %}
</select>