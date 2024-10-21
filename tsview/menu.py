def definition(**extra_urls):
    return [
        {
            'label': 'Navigation',
            'icone': 'bi bi-globe',
            'links': [
                {
                    'label': 'Home',
                    'icone': 'bi bi-house-fill',
                    'target': '/',
                    'id': 'navigation-home'
                },
                {
                    'label': 'Settings',
                    'icone': 'bi bi-gear',
                    'target': '/settings',
                    'id': 'monitor-settings'
                },
                {
                    'label': 'Dashboard',
                    'icone': 'bi bi-clipboard2-data',
                    'target': extra_urls['dashboard_url'],
                    'id': 'navigation-dashboard',
                    'profeature': True

                },
            ]
        },
        {
            'label': 'Timeseries',
            'icone': 'bi bi-graph-up-arrow',
            'links': [
                {
                    'label': 'Catalog',
                    'icone': 'bi bi-database',
                    'target': '/tssearch',
                    'id': 'timeseries-catalog'
                },
                {
                    'label': 'Quick View',
                    'icone': 'bi bi-lightning-charge',
                    'target': '/tsview',
                    'id': 'timeseries-quickview'
                },
                {
                    'label': 'Delete',
                    'icone': 'bi bi-trash',
                    'target': '/tsdelete',
                    'id': 'timeseries-delete'
                },
            ]
        },
        {
            'label': 'Formula',
            'icone': 'bi bi-diagram-3',
            'links': [
                {
                    'label': 'Documentation',
                    'icone': 'bi bi-info-circle',
                    'target': '/tsformula/operators',
                    'id': 'formula-documentation'
                },
                {
                    'label': 'List',
                    'icone': 'bi bi-list-columns',
                    'target': '/formulas',
                    'id': 'formula-catalog'
                },
                {
                    'label': 'Create',
                    'icone': 'bi bi-pencil',
                    'target': '/tsformula',
                    'id': 'formula-create'
                },
                {
                    'label': 'Update batch',
                    'icone': 'bi bi-file-earmark-arrow-up',
                    'target': '/addformulas',
                    'id': 'formula-batch'
                },
                {
                    'label': 'Setup cache',
                    'icone': 'bi bi-wrench',
                    'target': '/formulacache',
                    'id': 'formula-cache'
                },
            ]
        },
        {
            'label': 'Basket',
            'icone': "bi bi-basket3",
            'links': [
                {
                    'label': 'Edit',
                    'icone': 'bi bi-pencil',
                    'target': '/queryeditor',
                    'id': 'basket-edit'
                }
            ]
        },
        {
            'label': 'Monitoring',
            'icone': 'bi bi-heart-pulse',
            'links': [
                {
                    'label': 'Warnings',
                    'icone': 'bi bi-exclamation-triangle',
                    'target': '/tswatch/',
                    'id': 'monitor-tswatch',
                    'profeature': True
                },
                {
                    'label': 'Tasks',
                    'icone': 'bi bi-clock-history',
                    'target': '/tasks/',
                    'id': 'monitor-tasks'
                },
            ]
        },
    ]
