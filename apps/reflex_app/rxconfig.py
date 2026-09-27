import reflex as rx

config = rx.Config(
    app_name="reflex_app",
    backend_port=8001,
    api_url="http://localhost:8001",
    plugins=[
        rx.plugins.SitemapPlugin(),
        rx.plugins.TailwindV4Plugin(),
        rx.plugins.RadixThemesPlugin(
            theme=rx.theme(
                appearance="light",
                has_background=True,
                accent_color="jade",
            )
        ),
    ]
)