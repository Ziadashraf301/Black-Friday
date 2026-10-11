from pathlib import Path

from tests.helpers import mark_folder


def pytest_collection_modifyitems(config, items):
    mark_folder(items, Path(__file__).parent, "unit")
