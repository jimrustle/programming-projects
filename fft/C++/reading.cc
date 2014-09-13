#include "constants.h"

#include <vector>
#include <algorithm>

void get_signal(std::vector<double> &out, FILE *fp)
{
    out.clear();

    // read into buf 1024 16-bit values
    int16_t buf[NUM_DOTS];
    fread(buf, sizeof(int16_t), NUM_DOTS, fp);

    // copy buf into out
    out.insert(out.end(), buf, buf + NUM_DOTS);

    // scale by 256 and offset by 128 the values in out using a lambda
    std::transform(out.begin(), out.end(), out.begin(),
        [] (double x) {
            return x/256.0 + 128.0;
        }
    );
}

