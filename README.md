# Black Friday Sales Analysis 🛍️

[![R](https://img.shields.io/badge/R-276DC3?style=flat&logo=r&logoColor=white)](https://www.r-project.org/)
[![MySQL](https://img.shields.io/badge/MySQL-8.0-4479A1?style=flat&logo=mysql&logoColor=white)](https://www.mysql.com/)
[![Docker](https://img.shields.io/badge/Docker-2496ED?style=flat&logo=docker&logoColor=white)](https://www.docker.com/)

A comprehensive statistical analysis of Black Friday transaction data to generate actionable insights for marketing teams. This project leverages machine learning algorithms including Random Forest regression, hierarchical clustering, and association mining to identify customer patterns and optimize promotional strategies.

![Black Friday Analytics](https://github.com/Ziadashraf301/Black-Friday/assets/111798631/d7ed21a3-3456-40cd-8c17-6ceed2990fa0)

---

## 🎯 Project Overview

This analysis helps marketing teams make data-driven decisions for Black Friday campaigns by:

- 🔍 **Identifying high-value customers** for targeted retention and VIP programs
- 🔗 **Discovering product relationships** to create effective bundles and cross-sells
- 👥 **Segmenting customers** into actionable groups for personalized marketing
- 💰 **Optimizing pricing strategies** based on customer behavior and preferences

---

## 📊 Analysis Components

### 1. High-Value Customer Identification
**Goal:** Identify top customers to maximize retention and lifetime value

**Methods:**
- Purchase frequency analysis
- Total spending patterns
- Product preference profiling

**Business Impact:** Create targeted promotions, loyalty rewards, and VIP programs for the most valuable customers.

---

### 2. Product Association & Network Analysis
**Goal:** Uncover which products are frequently purchased together

**Methods:**
- Apriori algorithm for association rule mining
- PageRank & HITS for network centrality
- Market basket analysis

**Business Impact:** Design compelling product bundles, optimize inventory placement, and create strategic cross-selling opportunities.

---

### 3. Customer Segmentation
**Goal:** Group customers into distinct segments for personalized targeting

**Methods:**
- Hierarchical clustering algorithms
- Feature engineering (order frequency, total purchased, purchase value range)

**Segmentation Factors:**
- Demographics: age, gender, occupation
- Geographic: city category, duration in current city
- Behavioral: purchase patterns and preferences

**Business Impact:** Deliver personalized promotions and messaging that resonate with each customer segment.

---

### 4. Pricing & Promotion Strategy
**Goal:** Understand factors that drive purchase decisions

**Methods:**
- Random Forest regression
- Feature importance analysis
- Outlier detection

**Analysis Factors:**
- Product categories
- Brand preferences
- Price sensitivity
- Customer demographics

**Business Impact:** Create data-driven pricing strategies and promotions tailored to different customer segments.

---

## 🔬 Methodology

The project follows a structured 5-part analytical workflow:

### **Part 1: Data Preparation**
- Collect Black Friday transaction data
- Impute missing values using statistical methods
- Validate data quality
- Store cleaned data in MySQL database

### **Part 2: Exploratory Analysis**
- Perform descriptive statistical analysis
- Conduct hypothesis testing
- Identify patterns and trends
- Validate key assumptions about customer behavior

### **Part 3: Regression Analysis**
- Build Random Forest regression models
- Identify key drivers of purchase behavior
- Perform outlier analysis
- Validate model performance

### **Part 4: Customer Clustering**
- Engineer relevant features from transaction data
- Apply hierarchical clustering algorithms
- Profile and characterize customer segments
- Generate actionable segment insights

### **Part 5: Association Mining**
- Apply Apriori algorithm for frequent itemset mining
- Conduct network analysis using PageRank and HITS
- Identify product purchase patterns
- Generate product recommendation strategies

---

## 🛠️ Technology Stack

**Analytics & Machine Learning:**
- R programming language
- Random Forest (regression)
- Hierarchical Clustering
- Apriori Algorithm
- PageRank & HITS Algorithms

**Data Infrastructure:**
- MySQL 8.0
- Docker & Docker Compose

---

## 🚀 Getting Started

### Prerequisites
- Docker and Docker Compose
- R (version 4.0+)
- Git

### Quick Start

1. **Clone the repository**
   ```bash
   git clone https://github.com/Ziadashraf301/Black-Friday.git
   cd Black-Friday
   ```

2. **Launch MySQL database**
   ```bash
   docker-compose up -d
   ```

3. **Verify database**
   ```bash
   docker ps
   ```
   Confirm `mysql_fridayblack` is running on port 3306

4. **Run analysis**
   Execute R scripts sequentially (Part 1 → Part 5)

### Database Configuration

```yaml
Host: localhost
Port: 3306
Database: fridayblack
Root Password: 3012001
Features: Local infile enabled for bulk imports
```

**Stop database when finished:**
```bash
docker-compose down
```

---

## 📁 Project Structure

```
Black-Friday/
├── docker-compose.yml    # MySQL container configuration
├── init.sql             # Database initialization
├── README.md            # Project documentation
└── [Analysis Scripts]   # R scripts for 5-part analysis
```

---

## 👤 Author

**Ziad Ashraf**

- GitHub: [@Ziadashraf301](https://github.com/Ziadashraf301)
- Project: [Black-Friday](https://github.com/Ziadashraf301/Black-Friday)

---

## ⭐ Show Your Support

If you find this project helpful, please consider giving it a star!

---

**Built with ❤️ for data-driven marketing**