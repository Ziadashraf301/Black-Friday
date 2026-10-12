import sys
from pathlib import Path

from core.config import settings
from tests.helpers import mark_folder

# The Reflex app is imported as the top-level package `reflex_app` by the UI tests
sys.path.insert(0, str(settings.BASE_DIR / "apps" / "reflex_app"))


def pytest_collection_modifyitems(config, items):
    mark_folder(items, Path(__file__).parent, "unit")
