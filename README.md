# Assignment-5
Name: Makar Dorohuntsev <br>
Date: Apr 2 2025 <br>
Status: Bioinformatics student in UJ, Cracow

# Git Cheat Sheet

## Introduction to Git
Git is a distributed version control system that helps track code changes, collaborate on projects, and manage repositories efficiently.

## Essential Git Commands

### Setup
```bash
git config --global user.name "Your Name"
git config --global user.email "your@email.com"
git config --list
```

### Repository Management
```bash
git init  # Initialize a new repository
git clone REPOSITORY_URL  # Clone an existing repository
```

### Staging and Committing
```bash
git status  # Check status
git add .  # Stage all changes
git commit -m "Commit message"  # Commit changes
git log  # View commit history
```

### Branching
```bash
git branch  # List branches
git checkout -b BRANCH_NAME  # Create and switch to a new branch
git merge BRANCH_NAME  # Merge branches
```

### Remote Repositories
```bash
git remote add origin REPOSITORY_URL  # Add remote repository
git push origin BRANCH_NAME  # Push changes
git pull origin BRANCH_NAME  # Pull changes
```

### Undoing Changes
```bash
git checkout -- FILE_NAME  # Discard changes
git reset --soft COMMIT_ID  # Undo commits (keep changes)
git reset --hard COMMIT_ID  # Undo commits (discard changes)
```

## Summary
Git simplifies version control and collaboration. Mastering these commands will help manage projects efficiently. You can integrate Git with code editors like RStudio or VS Code and even use Colab to generate code and fix bugs. Git is crucial in company project management and tool developement. 
