# Lisp Foundations

This repository contains my study notes and exercises while learning Lisp, based on the book:

**Book:** *LISP - 3rd Edition*  
**Authors:** Patrick Henry Winston and Berthold Klaus Paul Horn

---

## 📂 Project Structure

The project is organized by chapter to ensure a structured learning path. Each chapter contains:

* `implementation.lisp`: Hands-on syntax exploration and core concepts transcribed directly from the textbook examples.
* `practices/`: Independent implementations of the end-of-chapter exercises.


```text
.
├── chapter03/ ...
├── chapter04/
│   ├── implementation.lisp    # Predicates & Conditionals
│   └── practices/
│       └── problem-4-1.lisp
├── ...
├── chapter11/
│   ├── implementation.lisp    # Properties and Arrays
│   └── practices/
│       └── problem-11-6.lisp  # Checkerboard Evaluation
└── ...
```

## 🛠 Execution & Environment

I use **SBCL (Steel Bank Common Lisp)** as the primary environment. To execute a specific script from the root directory, use the following command:

```bash
sbcl --script <path_to_file>.lisp
```

**Example:**
```bash
sbcl --script chapter11/practices/problem-11-6.lisp
```
---
*Maintained by **Minh Dieu Vu** - Faculty of Mathematics and Computer Science (VNUHCM-University of Science)*