# Taken from:
# 	https://www.geeksforgeeks.org/convert-decimal-fraction-binary-number/
# 
# DMD, 170920

def decimalToBinary(num, k_prec):
    """
    decimalToBinary returns a truncation of the
    binary representation of a floating point
    number as a string.

    Parameters
    ----------
    num : float
            The floating point number for
            which we want the binary expansion.
    k_prec : int
            The number of decimal places of the
            binary expansion to return.

    Returns
    -------
    binary : str
            The binary expansion of num to
            k_prec fractional bits.
    """

    binary = ""

    # Fetch the integral part of
    # decimal number
    Integral = int(num)

    # Fetch the fractional part
    # decimal number
    fractional = num - Integral

    # Conversion of integral part to
    # binary equivalent
    while (Integral) :
        rem = Integral % 2

        # Append 0 in binary
        binary += str(rem);

        Integral //= 2
   
    # Reverse string to get original
    # binary equivalent
    binary = binary[ : : -1]

    # Append point before conversion
    # of fractional part
    binary += '.'

    # Conversion of fractional part
    # to binary equivalent
    while (k_prec) :
        # Find next bit in fraction
        fractional *= 2
        fract_bit = int(fractional)

        if (fract_bit == 1) :
           
            fractional -= fract_bit
            binary += '1'
           
        else :
            binary += '0'

        k_prec -= 1

    return binary