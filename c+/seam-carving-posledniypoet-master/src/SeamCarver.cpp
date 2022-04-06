#include "SeamCarver.h"

#include <cmath>
#include <algorithm>

double get_pixel_energy(const Image &image, size_t columnId, size_t rowId) {
    size_t width = image.m_table.size();
    size_t height = image.m_table[0].size();
    Image::Pixel left = image.GetPixel((columnId + width - 1) % width, rowId);
    Image::Pixel right = image.GetPixel((columnId + 1) % width, rowId);
    Image::Pixel up = image.GetPixel(columnId, (rowId + height - 1) % height);
    Image::Pixel down = image.GetPixel(columnId, (rowId + 1) % height);
#define sqr(x) ((x) * (x))
    return std::sqrt(sqr(left.m_red - right.m_red) +
                     sqr(left.m_blue - right.m_blue) +
                     sqr(left.m_green - right.m_green) +
                     sqr(up.m_red - down.m_red) +
                     sqr(up.m_blue - down.m_blue) +
                     sqr(up.m_green - down.m_green));
#undef sqr
}

Image transpose(const Image &image) {
    std::vector<std::vector<Image::Pixel>> result;
    result.resize(image.m_table[0].size());
    for (size_t row = 0; row < image.m_table[0].size(); ++row) {
        for (size_t col = 0; col < image.m_table.size(); ++col) {
            result[row].emplace_back(image.GetPixel(col, row));
        }
    }
    return result;
}

std::vector<size_t> find_horizontal_seam(const Image &image) {
    size_t height = image.m_table[0].size();
    size_t width = image.m_table.size();
    std::vector<std::vector<double>> min_sum(width);
    std::vector<std::vector<size_t>> from(width);
    for (size_t col = 0; col < width; ++col) {
        min_sum[col].resize(height);
        from[col].resize(height);
    }
    for (size_t i = 0; i < height; ++i) {
        min_sum[0][i] = get_pixel_energy(image, 0, i);
    }
    for (size_t col = 1; col < width; ++col) {
        for (size_t row = 0; row < height; ++row) {
            min_sum[col][row] = min_sum[col - 1][row];
            from[col][row] = row;
            if (row > 0 && min_sum[col][row] > min_sum[col - 1][row - 1]) {
                min_sum[col][row] = min_sum[col - 1][row - 1];
                from[col][row] = row - 1;
            }
            if (row + 1 < height && min_sum[col][row] > min_sum[col - 1][row + 1]) {
                min_sum[col][row] = min_sum[col - 1][row + 1];
                from[col][row] = row + 1;
            }
            min_sum[col][row] += get_pixel_energy(image, col, row);
        }
    }
    std::vector<size_t> result(1);
    result[0] = 0;
    double sum = min_sum[width - 1][0];
    for (size_t i = 1; i < height; ++i) {
        if (min_sum[width - 1][i] < sum) {
            result[0] = i;
            sum = min_sum[width - 1][i];
        }
    }
    for (size_t col = width - 1; col > 0; --col) {
        result.emplace_back(from[col][result.back()]);
    }
    std::reverse(result.begin(), result.end());
    return result;
}

void remove_horizontal_seam(Image &image, const std::vector<size_t> &seam) {
    for (size_t col = 0; col < image.m_table.size(); ++col) {
        auto it = image.m_table[col].begin();
        for (size_t i = 0; i < seam[col]; ++i) {
            ++it;
        }
        image.m_table[col].erase(it);
    }
}

SeamCarver::SeamCarver(Image image)
        : m_image(std::move(image))
{}

const Image& SeamCarver::GetImage() const
{
    return m_image;
}

size_t SeamCarver::GetImageWidth() const
{
    return m_image.m_table.size();
}

size_t SeamCarver::GetImageHeight() const
{
    return m_image.m_table[0].size();
}

double SeamCarver::GetPixelEnergy(size_t columnId, size_t rowId) const
{
    return get_pixel_energy(m_image, columnId, rowId);
}

SeamCarver::Seam SeamCarver::FindHorizontalSeam() const
{
    return find_horizontal_seam(m_image);
}

SeamCarver::Seam SeamCarver::FindVerticalSeam() const
{
    return find_horizontal_seam(transpose(m_image));
}

void SeamCarver::RemoveHorizontalSeam(const Seam& seam)
{
    remove_horizontal_seam(m_image, seam);
}

void SeamCarver::RemoveVerticalSeam(const Seam& seam)
{
    m_image = transpose(m_image);
    remove_horizontal_seam(m_image, seam);
    m_image = transpose(m_image);
}
