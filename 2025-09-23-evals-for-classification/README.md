# 🦄 ai that works: Evals for Classification

> In this episode, hosts Vaibhav Gupta and Dex, along with guest Kevin Gregory, explore building production-ready AI classification systems, focusing on evaluation, tuning, and user experience design.

[Video](https://youtu.be/5Fy0hBzyduU) (1h27m)

[![Evals for Classification](https://img.youtube.com/vi/5Fy0hBzyduU/0.jpg)](https://youtu.be/5Fy0hBzyduU)


## Episode Overview

This episode dives deep into the practical challenges of building AI systems ready for production. The hosts explore large-scale classification systems handling 1000+ categories, demonstrating how to evaluate and tune these systems for real-world use.

<img width="888" height="554" alt="Screenshot 2025-10-04 at 11 56 00 AM" src="https://github.com/user-attachments/assets/c3bd1bfa-c83e-4607-a10b-793699406388" />


<img width="942" height="581" alt="Screenshot 2025-10-04 at 11 55 50 AM" src="https://github.com/user-attachments/assets/bb097f0f-dce9-4a63-a352-d764671f1d14" />


### Key Topics Covered

- Building production-grade classification systems
- Dynamic UIs for flexible content creation
- Using LLMs to enhance classification accuracy
- Evaluation strategies and custom dashboards
- The subjective nature of classification correctness
- Tuning classification pipelines for performance
- Balancing accuracy, cost, and user experience

## Key Takeaways

- AI engineering concepts can be applied to real projects with measurable impact
- Building production-grade classification systems requires careful attention to UX
- Evaluating AI systems requires understanding both metrics and user experience
- Subjectivity plays a significant role in defining correct classifications
- Real user data is crucial for effective iteration and improvement
- UI design should prioritize clarity and enable rapid spot-checking
- Iterative development accelerates the path to working solutions
- Metrics should tie back to business outcomes for meaningful evaluation
- Model upgrades and user feedback drive continuous improvement
- Engineers must balance accuracy and cost in AI solutions

## Episode Highlights

> "The most important thing is to make it work quickly and iterate with real user data."

> "Building a UI is essential - it's not just about the model, it's about how users interact with your classification system."

> "Understanding what 'correct' means for your specific use case is more important than achieving perfect accuracy on arbitrary benchmarks."

## Resources

- [Session Recording](https://youtu.be/5Fy0hBzyduU)
- [Discord Community](https://boundaryml.com/discord)
- Sign up for the next session on [Luma](https://lu.ma/baml)

## Whiteboards

---


> A production-ready AI classification system that handles 1000+ categories using a various approaches combining embeddings and LLM-based selection.

[Original Video Tutorial](https://youtu.be/6B7MzraQMZk)

[![Large Scale Classification](https://img.youtube.com/vi/6B7MzraQMZk/0.jpg)](https://www.youtube.com/watch?v=6B7MzraQMZk)

## Overview

This system solves the challenge of classifying text into large category sets (1000+ categories) by using a two-stage approach:

1. **Narrowing Stage**: Uses vector embeddings to quickly narrow down from 1000+ categories to ~5-10 candidates
2. **Selection Stage**: Uses LLM reasoning to select the best final category from the narrowed candidates

## Quick Start

### Prerequisites

- Python 3.10+
- OpenAI API key
- UV package manager

### Installation

```bash
# Clone and navigate to the project
cd level-3-code/large_scale_classification

# Install runtime dependencies (for running the system)
uv sync

# OR install with development dependencies (for contributing/development)
uv sync --extra dev

# Set up environment variables
cp .env.example .env
# Edit .env and add your OPENAI_API_KEY
```

> **Note**: Use `uv sync --extra dev` if you plan to contribute to the project or need development tools like linting (ruff), type checking (pyright), and testing (pytest). For just running the classification system, `uv sync` is sufficient.

### Generate BAML Client

```bash
# Convert BAML files to Python client code
uv run baml-cli generate
```

### Basic Usage

```bash
# Run the interactive classification system
uv run python src/main.py
```

This will prompt you to enter text for classification and return the most appropriate category.

## Architecture

### Core Components

The system is built with a modular architecture:

```
src/
├── main.py                    # Entry point
├── classification/            # Core classification logic
│   ├── pipeline.py           # Main orchestrator
│   ├── embeddings.py         # OpenAI embedding service
│   ├── narrowing.py          # Category narrowing strategies
│   ├── selection.py          # LLM-based final selection
│   └── vector_store.py       # ChromaDB vector store
├── data/                     # Data management
│   ├── category_loader.py    # Category loading and processing
│   └── models.py             # Pydantic data models
├── config/                   # Configuration
│   └── settings.py           # Application settings
└── shared/                   # Shared utilities
    ├── logger.py             # Structured logging
    ├── constants.py          # Application constants
    └── enums.py              # Enums and types
```

### Classification Pipeline

1. **Text Input**: User provides text to classify
2. **Category Loading**: System loads 1000+ categories from `data/categories.txt`
3. **Embedding Generation**: Creates embeddings for input text and categories
4. **Narrowing**: Reduces categories to top candidates using similarity search
5. **LLM Selection**: Uses BAML/LLM to choose the best category from candidates
6. **Result**: Returns selected category with metadata and timing

## Performance Features

### Vector Store Caching

The system includes an advanced ChromaDB-based vector store for performance:

- **Faster lookups**: Cached embeddings vs fresh API calls
- **Automatic caching**: New categories are automatically added to the store
- **Model validation**: Ensures compatibility between stored and current embeddings

#### Build Vector Store

```bash
# Build the vector store from categories
python scripts/build_vector_store.py

# Force rebuild (e.g., after changing embedding models)
python scripts/build_vector_store.py --force-rebuild
```

### Narrowing Strategies

The system supports multiple narrowing strategies:

- **Embedding**: Pure embedding similarity (fastest)
- **Hybrid**: Embedding + LLM reasoning (most accurate, default)
- **LLM**: Pure LLM-based narrowing (most flexible)

Configure in `src/config/settings.py`:

```python
narrowing_strategy = NarrowingStrategy.HYBRID  # EMBEDDING, HYBRID, or LLM
max_narrowed_categories = 5  # Number of candidates to pass to final selection
```

## Testing

The system includes comprehensive testing infrastructure with both unit and integration tests:

### Run Tests

```bash
# Run all tests (unit + integration)
cd tests
python run_tests.py

# Run specific test types
python run_tests.py --unit                  # Unit tests only (fast, no API calls)
python run_tests.py --narrowing-accuracy    # Narrowing accuracy integration test
python run_tests.py --selection-accuracy    # Selection accuracy integration test
python run_tests.py --pipeline-accuracy     # Complete pipeline integration test
python run_tests.py --all                   # All tests explicitly
```

### Test Types

- **Unit Tests**: Fast component testing with mocking (embeddings, narrowing, selection, pipeline, vector store)
- **Narrowing Accuracy**: Tests how often the correct category is included in narrowed results
- **Selection Accuracy**: Tests final category selection accuracy  
- **Pipeline Accuracy**: End-to-end pipeline testing with performance metrics

### Test Results

Integration tests automatically save detailed JSON results with timestamps for performance tracking:

```bash
# Compare results across test runs
python tests/compare_results.py --narrowing file1.json file2.json
```

### Running Individual Tests

```bash
# Unit tests (from project root)
uv run pytest tests/unit/classification/pipeline_test.py -v
uv run pytest tests/unit/classification/selection_test.py -v

# Integration tests (from tests/integration)
cd tests/integration
python test_pipeline_accuracy.py
```

## Configuration

### Environment Variables

Create a `.env` file with only the required API key:

```bash
# Required - the only thing needed in .env
OPENAI_API_KEY=your_api_key_here
```

### Application Settings

All other configuration is done in `src/config/settings.py`. You can modify the default values directly in the file:

```python
class Settings(BaseSettings):
    """Application configuration settings."""

    # OpenAI Configuration
    openai_api_key: str  # Loaded automatically from .env. Don't put your key here
    embedding_model: str = "text-embedding-3-small"
    
    # Classification Strategy
    narrowing_strategy: NarrowingStrategy = NarrowingStrategy.HYBRID
    max_narrowed_categories: int = 5
    
    # Hybrid Strategy Specific Settings
    max_embedding_candidates: int = 10  # How many categories embedding stage returns
    max_final_categories: int = 3       # How many categories LLM stage returns
    
    # Data Configuration
    categories_file_path: pathlib.Path = CWD.parents[1] / C.DATA / C.CATEGORIES_TXT
```

### Available Narrowing Strategies

Configure `narrowing_strategy` in settings.py:

- `NarrowingStrategy.HYBRID`: Embedding + LLM reasoning (most accurate, default)
- `NarrowingStrategy.LLM`: Pure LLM-based narrowing (most flexible)

### Tuning Performance

Adjust these settings in `settings.py` to optimize for your use case:

- `max_narrowed_categories`: Number of candidates passed to final selection (default: 5)
- `max_embedding_candidates`: For hybrid strategy, how many categories the embedding stage returns (default: 10)
- `max_final_categories`: For hybrid strategy, how many categories the LLM stage returns (default: 3)
- `embedding_model`: OpenAI embedding model to use (default: "text-embedding-3-small")

### Category Data

Categories are loaded from `data/categories.txt`. The format supports hierarchical categories:

```
/Appliances/Refrigerators/French Door Refrigerators
/Appliances/Dishwashers/Built-in Dishwashers
/Appliances/Appliance Parts/Dishwasher Parts
```

## 🔄 Development Workflow

### Configuration → Testing → Analysis Workflow

The system supports a complete development workflow for optimizing classification performance:

1. **Update Configuration**: Modify settings in `src/config/settings.py`
2. **Run Performance Tests**: Execute pipeline tests with version tracking
3. **Analyze Results**: Use the Streamlit app to compare performance across versions

### Example Workflow

```bash
# 1. Update configuration settings
# Edit src/config/settings.py - for example:
#   max_narrowed_categories = 10  (was 5)
#   max_embedding_candidates = 50  (was 10)

# 2. Run pipeline test with version tracking
uv run python tests/integration/test_pipeline_accuracy.py --save-as v7 --description "embedding 50, llm 10, model upgrade"

# 3. View results in Streamlit app
uv run streamlit run ui/app.py

# 4. Compare with previous versions in the UI
# The app will show performance comparisons across all saved versions
```

### Configuration Parameters for Optimization

Key settings in `src/config/settings.py` that affect performance:

```python
class Settings(BaseSettings):
    # Strategy Selection
    narrowing_strategy: NarrowingStrategy = NarrowingStrategy.HYBRID
    
    # Performance Tuning
    max_narrowed_categories: int = 5        # Final candidates passed to LLM
    max_embedding_candidates: int = 10      # Embedding stage candidates (hybrid only)
    max_final_categories: int = 3           # LLM stage candidates (hybrid only)
    
    # Model Selection
    embedding_model: str = "text-embedding-3-small"  # or "text-embedding-3-large"
```

### Streamlit Analysis Dashboard

The Streamlit app (`ui/app.py`) provides:

- **Performance Comparison**: Compare accuracy and timing across test versions
- **Detailed Analysis**: Drill down into individual test case results
- **Configuration Tracking**: See what settings were used for each version
- **Trend Analysis**: Track performance improvements over time

Launch the dashboard:
```bash
uv run streamlit run ui/app.py
```

### Version Management

Pipeline tests support version tracking for systematic performance analysis:

```bash
# Save test results with version and description
uv run python tests/integration/test_pipeline_accuracy.py --save-as v6 --description "baseline configuration"
uv run python tests/integration/test_pipeline_accuracy.py --save-as v7 --description "increased embedding candidates to 50"
uv run python tests/integration/test_pipeline_accuracy.py --save-as v8 --description "upgraded to text-embedding-3-large"
```

Results are saved to `tests/results/saved_runs/` with metadata for easy comparison.

## 🔧 Advanced Usage

### Programmatic Usage

```python
from src.classification.pipeline import ClassificationPipeline

# Initialize pipeline
pipeline = ClassificationPipeline()

# Classify text
result = pipeline.classify("Samsung 17.5-cu ft French door refrigerator")

print(f"Category: {result.category.name}")
print(f"Confidence: {result.confidence}")
print(f"Processing time: {result.processing_time_ms:.1f}ms")
print(f"Candidates: {[c.name for c in result.candidates]}")
```

### Custom Categories

To use your own category set:

1. Replace `data/categories_full.txt` with your categories
2. Rebuild the vector store: `python scripts/build_vector_store.py --force-rebuild`
3. Update test cases in `tests/data/test_cases.py` if needed

### BAML Integration

The system uses [BAML](https://docs.boundaryml.com/) for LLM interactions. BAML files are in `src/baml_src/`:

- `clients.baml`: LLM client configurations
- `pick_best_category.baml`: Category selection prompt
- `generators.baml`: Type definitions

## Development

### Adding New Features

The modular architecture makes it easy to extend:

1. **New Narrowing Strategy**: Inherit from `NarrowingStrategy` in `narrowing.py`
2. **Custom Embedding Models**: Modify `EmbeddingService` in `embeddings.py`
3. **Additional Metadata**: Extend `ClassificationResult` in `models.py`

### Code Quality

- **Type Safety**: Full Pydantic models and type hints
- **Logging**: Structured logging with performance metrics
- **Error Handling**: Comprehensive exception handling
- **Testing**: Unit, integration, and accuracy tests

---

Built with ❤️ using BAML, OpenAI, ChromaDB, and Python, but especially BAML.
