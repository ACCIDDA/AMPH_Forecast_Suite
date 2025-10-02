# Contributing to AMPH Forecast Suite

Thank you for your interest in contributing to the AMPH Forecast Suite! We welcome contributions from the community.

## How to Contribute

### Reporting Bugs

If you find a bug, please open an issue on GitHub with:
- A clear description of the bug
- Steps to reproduce the behavior
- Expected behavior
- Your R version and operating system

### Suggesting Enhancements

We welcome suggestions for new features or enhancements. Please open an issue with:
- A clear description of the enhancement
- Why it would be useful
- Example use cases

### Pull Requests

1. Fork the repository
2. Create a new branch for your feature (`git checkout -b feature/AmazingFeature`)
3. Make your changes
4. Add tests for your changes
5. Update documentation as needed
6. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
7. Push to the branch (`git push origin feature/AmazingFeature`)
8. Open a Pull Request

### Code Style

- Follow the tidyverse style guide
- Use roxygen2 for documentation
- Include examples in function documentation
- Write tests for new functions

### Documentation

- Update README.md if you change functionality
- Update NEWS.md with your changes
- Ensure all functions have proper roxygen2 documentation
- Update vignettes if adding major new features

## Development Setup

1. Clone the repository
2. Install development dependencies:
   ```r
   install.packages(c("devtools", "roxygen2", "testthat", "knitr", "rmarkdown"))
   ```
3. Load the package for development:
   ```r
   devtools::load_all()
   ```
4. Run tests:
   ```r
   devtools::test()
   ```
5. Build documentation:
   ```r
   devtools::document()
   ```

## Questions?

Feel free to open an issue with any questions about contributing.
