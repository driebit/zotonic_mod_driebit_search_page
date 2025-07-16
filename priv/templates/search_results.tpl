{% with m.search.query::%{ qargs: true, page: q.page } as result %}
    <h2>Page #{{ result.page }}</h2>

    <ul>
        {% for id in result %}
            <li><a href="{{ id.page_url }}">{{ id.title }}</a></li>
        {% endfor %}
    </ul>

    {% pager dispatch="none"
             result=result
             topic="model/location/post/push"
             qargs
    %}
{% endwith %}