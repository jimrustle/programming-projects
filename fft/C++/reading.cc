#include "constants.h"

#include <vector>
#include <algorithm>

void get_signal(std::vector<double> &out, FILE *fp)
{
    out.clear();
    int16_t buf[NUM_DOTS];
    fread(buf, sizeof(int16_t), NUM_DOTS, fp);
    out.insert(out.end(), buf, buf + NUM_DOTS);
    std::transform(out.begin(), out.end(), out.begin(), [](double x) {
        return x/256.0 + 128.0;
    });
}
