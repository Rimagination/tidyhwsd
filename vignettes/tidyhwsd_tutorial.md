# 告别繁琐：用 tidyhwsd 优雅提取全球土壤数据

> **一行代码，获取 FAO HWSD v2.0 全球土壤属性。**

---

## 📌 写在前面

做土壤研究、生态建模、农业遥感的朋友，一定对 **HWSD（Harmonized World Soil Database）** 不陌生。这是 FAO 官方发布的全球土壤数据库，覆盖全球、分层详细、属性丰富。

但问题来了：

- 原始数据是 Access 数据库 + BIL 栅格，处理起来相当麻烦
- 想提取某个点的土壤属性？得先读栅格、再查表、再匹配……
- 想批量处理几百个样点？循环写到头秃

**tidyhwsd** 就是为了解决这个痛点而生的 R 包。它把 HWSD v2.0 的数据预处理好，提供简洁的 tidyverse 风格 API，让你**一行代码就能提取想要的土壤属性**。

---

## 🚀 快速上手

### 1️⃣ 安装

```r
# 安装依赖（首次使用）
install.packages(c("terra", "sf", "dplyr", "cli", "tibble", "rlang", "httr"))

# 从 GitHub 安装 tidyhwsd
remotes::install_github("Rimagination/tidyhwsd")
```

### 2️⃣ 下载索引栅格（仅需一次）

HWSD 的核心是一个索引栅格（SMU ID），包需要它来定位土壤单元。第一次使用时下载一次即可：

```r
library(tidyhwsd)

# 下载到本地目录（约 100 MB）
hwsd_download(ws_path = "~/data/HWSD2", verbose = TRUE)
```

> 💡 **小技巧**：在 `~/.Renviron` 中添加 `WS_PATH=~/data/HWSD2`，之后就不用每次指定路径了。

---

## 🎯 核心用法

### 单点提取

给一个经纬度，返回土壤属性值：

```r
pt <- hwsd_extract(
  coords  = c(110, 40),          # 经度, 纬度
  param   = c("SAND", "PH_WATER"),
  layer   = "D1",                # 表层土壤
  ws_path = "~/data/HWSD2"
)
pt
```

**输出：**

```
# A tibble: 2 × 4
  longitude latitude parameter value
      <dbl>    <dbl> <chr>     <dbl>
1       110       40 SAND         41
2       110       40 PH_WATER    7.4
```

---

### 批量点位提取

实际项目中，往往需要处理几十上百个样点。只需传入 data.frame 即可：

```r
library(tibble)

# 准备样点数据
sites <- tibble(
  lon = c(116.4, 121.5, 113.3, 104.1),
  lat = c(39.9,  31.2,  23.1,  30.7)
)

# 批量提取
pts <- hwsd_extract(
  coords  = sites,
  param   = c("SAND", "CLAY", "OC"),
  layer   = "D1",
  ws_path = "~/data/HWSD2"
)
pts
```

**输出：**

```
# A tibble: 12 × 4
   longitude latitude parameter value
       <dbl>    <dbl> <chr>     <dbl>
 1     116.4     39.9 SAND         36
 2     116.4     39.9 CLAY         28
 3     116.4     39.9 OC          1.2
 4     121.5     31.2 SAND         42
 ...
```

返回的是整洁的 **long format**，方便后续用 `pivot_wider()` 转换或直接 `ggplot()` 可视化。

---

### 区域提取（栅格输出）

需要提取一个区域的土壤属性？用 `bbox` 参数，返回 `SpatRaster`：

```r
# 中国范围
sand_china <- hwsd_extract(
  bbox     = c(70, 18, 140, 54),
  param    = "SAND",
  layer    = "D1",
  ws_path  = "~/data/HWSD2",
  tiles_deg = 5,    # 分块处理，避免内存爆炸
  cores     = 4     # 并行加速（非 Windows）
)

# 快速预览
terra::plot(sand_china)
```

![sand_china](sand_china_preview.png)

保存为 GeoTIFF：

```r
terra::writeRaster(sand_china, "~/data/sand_D1_china.tif", overwrite = TRUE)
```

---

## 📊 可视化：ggplot2 + tidyterra

想用 ggplot2 画出更精美的土壤分布图？配合 **tidyterra** 包：

```r
library(ggplot2)
library(tidyterra)

ggplot() +
  geom_spatraster(data = sand_china) +
  scale_fill_viridis_c(
    name = "Sand (%)",
    na.value = "grey90"
  ) +
  labs(
    title = "中国表层土壤砂粒含量",
    subtitle = "数据来源：HWSD v2.0 (Layer D1)",
    caption = "Powered by tidyhwsd"
  ) +
  theme_minimal()
```

---

## 📝 常用土壤属性速查

| 参数名 | 含义 | 单位 |
|--------|------|------|
| `SAND` | 砂粒含量 | % |
| `SILT` | 粉粒含量 | % |
| `CLAY` | 黏粒含量 | % |
| `OC` | 有机碳 | % |
| `PH_WATER` | pH（水提） | - |
| `CEC` | 阳离子交换量 | cmol/kg |
| `BD` | 容重 | g/cm³ |

完整属性列表：

```r
hwsd_props()
```

---

## 🔧 进阶技巧

### 设置环境变量（省去每次指定路径）

在 `~/.Renviron` 中添加：

```
WS_PATH=~/data/HWSD2
```

重启 R 后，直接调用即可：

```r
pt <- hwsd_extract(coords = c(120, 30), param = "SAND", layer = "D1")
```

### 土层说明

HWSD v2.0 提供 7 个深度层（D1–D7）：

| 层代码 | 深度范围 |
|--------|----------|
| D1 | 0–20 cm |
| D2 | 20–40 cm |
| D3 | 40–60 cm |
| D4 | 60–80 cm |
| D5 | 80–100 cm |
| D6 | 100–150 cm |
| D7 | 150–200 cm |

---

## 🤔 常见问题

**Q: 为什么提取结果全是 NA？**

检查以下几点：
1. 坐标是否在陆地上（海洋区域无数据）
2. `ws_path` 路径是否正确，是否包含 `HWSD2.bil` 文件
3. `layer` 参数是否正确（D1–D7）

**Q: 为什么会有负值（如 -9）？**
HWSD 使用 -9 等负值表示无效数据（如水体、城市）。`tidyhwsd` 0.2.0 版本起会自动将其转换为 `NA`。

**Q: 支持哪些坐标格式？**

- 单点：`c(lon, lat)`
- 多点矩阵：2 列的 matrix
- data.frame/tibble：需包含 `lon` 和 `lat` 列

**Q: 大范围提取很慢怎么办？**

使用 `tiles_deg` 分块 + `cores` 并行：

```r
hwsd_extract(
  bbox = c(...),
  tiles_deg = 5,
  cores = 8
)
```

---

## 🔗 相关资源

- **GitHub 仓库**：[Rimagination/tidyhwsd](https://github.com/Rimagination/tidyhwsd)
- **HWSD v2.0 官网**：[FAO HWSD](https://www.fao.org/soils-portal/soil-survey/soil-maps-and-databases/harmonized-world-soil-database-v20/en/)
- **terra 包文档**：[rspatial.github.io/terra](https://rspatial.github.io/terra/)

---

## ✨ 结语

**tidyhwsd** 的设计理念很简单：**让繁琐的数据预处理消失，让你专注于科学问题本身**。

如果这个包对你的研究有帮助，欢迎 Star ⭐ 支持！有任何问题或建议，也欢迎在 GitHub 提 Issue。

---

*作者：Rimagination*  
*最后更新：2026-01-26*
