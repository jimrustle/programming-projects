import java.util.Random;

public class HugeInteger {
    private boolean sign = false;
    private String value = "";
    private int numDigits = 0;

    public HugeInteger(String stringVal) throws NumberFormatException {
        boolean b = stringVal.matches("-?\\d*");

        // regexp match for - and digits
        if (!b) {
            System.out.println(stringVal);
            throw new NumberFormatException("illegal characters in string value");
        }

        // if - appears, then it is negative
        if (stringVal.charAt(0) == '-') {
            sign = true;
            value = stringVal.substring(1, stringVal.length());
            numDigits = stringVal.length() - 1;
        }
        else {
            value = stringVal;
            numDigits = stringVal.length();
        }

        // remove all leading zeros
        __compress__();
    }

    public HugeInteger(int n) throws IllegalArgumentException {
        if (n < 1) throw new
            IllegalArgumentException("arg must be greater than one");

        Random r = new Random();

        // generate last n-1 digits
        for (int i = 0; i < n - 1; i++) {
            value += r.nextInt(10);
        }

        // first digit cannot be a 0
        while (true) {
            int k = r.nextInt(10);
            if (k != 0) {
                value = k + value;
                break;
            }
        }

        numDigits = n;
    }

    public HugeInteger add(HugeInteger h) {
        String top = value;
        String bottom = h.value;
        String result = "";

        // pad the shorter number with 0s
        int diff = Math.abs(numDigits - h.numDigits);
        if (h.numDigits > numDigits) {
            for (int i = 0; i < diff; i++) {
                top = "0" + top;
            }
        }
        else if (numDigits > h.numDigits) {
            for (int i = 0; i < diff; i++) {
                bottom = "0" + bottom;
            }
        }

        // different signs
        if (sign ^ h.sign) {
            // subtract using char arrays, since Java strings are immutable
            char[] tmp;
            char[] t = top.toCharArray();
            char[] b = bottom.toCharArray();
            char[] ret = new char[t.length];

            if (compareTo(h.__negate__()) == 0) {
                return new HugeInteger("0");
            }
            else if (compareTo(h) == -1) {
                tmp = b;
                b = t;
                t = tmp;
            }

            int length = t.length;
            int borrow = 0;
            boolean neg = false;
            // line up on the right and iterate
            for (int i = length - 1; i >= 0; i--) {
                diff = t[i] - b[i]; // subtract
                // borrow if difference is less than 0
                if (diff < 0) {
                    if (i == 0) {
                        diff = Math.abs(diff);
                        neg = true;
                    }
                    else {
                        t[i-1] -= 1;
                        diff += 10;
                    }
                }

                ret[i] = (char) ('0' + diff);
            }
            result = new String(ret);
            if (neg) result = "-" + result;

            return new HugeInteger(result);
        }

        // same signs
        else {
            // add string representation from the "aligned" right side
            int length = top.length();
            int carry = 0;
            for (int i = length - 1; i >= 0; i--) {
                int r = top.charAt(i) + bottom.charAt(i) - 2*'0' + carry;
                carry = (r > 9)? 1 : 0;
                result = (r % 10) + result;
            }

            // if carry still exists, add one to our number on the left
            if (carry == 1) {
                result = "1" + result;
            }

            // set the sign: (-a + -b) = -(a + b)
            if (sign || h.sign) result = "-" + result;

            return new HugeInteger(result);
        }
    }

    public HugeInteger subtract(HugeInteger h) {
        int c = compareTo(h);
        if (c == 0) return new HugeInteger("0");
        else if (c == 1) return add(h.__negate__());
        else return (h.add(this.__negate__())).__negate__();
    }

    public HugeInteger multiply(HugeInteger h) {
        String returnString;
        char[] ret = new char[numDigits + h.numDigits];
        char[] a = value.toCharArray();
        char[] b = h.value.toCharArray();

        // multiply
        for (int i = 0; i < numDigits; i++) {
            for (int j = 0; j < h.numDigits; j++) {
                ret[i+j] += (a[numDigits - 1 - i] - '0') *
                    (b[h.numDigits - 1 - j] - '0');
            }
        }

        // carry
        for (int i = 0; i < numDigits + h.numDigits; i++) {
            if (ret[i] > 9) {
                int carry = ret[i] / 10;
                ret[i] %= 10;
                ret[i+1] += carry;

            }
            // convert to ASCII
            ret[i] += '0';
        }

        // reverse the char array
        for (int i = 0,  j = numDigits + h.numDigits - 1; i < j; i++, j--) {
            char c = ret[i];
            ret[i] = ret[j];
            ret[j] = c;
        }

        returnString = new String(ret);

        // set the sign
        // a * b is negative if and only if one of the values is negative
        if (sign ^ h.sign) return new HugeInteger("-" + returnString);
        else return new HugeInteger(returnString);
    }

    public int compareTo(HugeInteger h) {
        // check signs
        // both negative
        if (sign && h.sign) {
            // check number of digits
            if (numDigits > h.numDigits) return -1;
            else if (numDigits < h.numDigits) return 1;
            // same number of digits means checking each one individually
            else {
                for (int i = 0; i < numDigits; i++) {
                    if (value.charAt(i) > h.value.charAt(i)) return -1;
                    else if (value.charAt(i) < h.value.charAt(i)) return 1;
                }
                return 0;
            }
        }
        // one negative xor one positive
        else if (sign ^ h.sign) {
            // if first is negative, it is smaller
            if (sign) return -1;
            else return 1;
        }
        // both positive
        else {
            // check number of digits
            if (numDigits > h.numDigits) return 1;
            else if (numDigits < h.numDigits) return -1;
            // same number of digits means checking each one individually
            else {
                for (int i = 0; i < numDigits; i++) {
                    if (value.charAt(i) > h.value.charAt(i)) return 1;
                    else if (value.charAt(i) < h.value.charAt(i)) return -1;
                }
                return 0;
            }
        }
    }

    public String toString() {
        // if negative, append a '-'
        if (sign)
            return "-" + value;
        else
            return value;
    }

    private HugeInteger __negate__() {
        HugeInteger ret = new HugeInteger(value);
        ret.sign = sign^true;
        return ret;
    }

    private void __compress__() {
        if (numDigits > 1)
            for (char c : value.toCharArray())
                if (c == '0') { numDigits--; value = value.substring(1, value.length());}
                else break;
    }

    public void __print_debug__() {
        System.out.println("Value: " + toString() + ", numDigits = " +  numDigits + ", sign = " + sign);
    }
}

