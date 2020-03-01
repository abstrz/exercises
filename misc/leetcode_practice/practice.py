class Solution:
    def sortLists(self, l1, l2):
        def recursively(result, l1, l2):
            if l1 == []:
                result += l2
                return result
            elif l2 == []:
                result += l1
                return result
            elif l1[0]<l2[0]:
                result.append(l1[0])
                return recursively(result, l1[1:], l2)
            elif l2[0]<l1[0]:
                result.append(l2[0])
                return recursively(result, l1, l2[1:])
            else:
                result.append(l1[0])
                result.append(l2[0])
                return recursively(result, l1[1:], l2[1:])
        return recursively([], l1, l2)

    def findMedianSortedArrays(self, nums1, nums2):
        sorted_nums = self.sortLists(nums1, nums2)
        lngth = len(sorted_nums);
        mid = int(lngth/2)
        if (lngth % 2 == 1):
            return sorted_nums[mid]
        else:
            return (sorted_nums[mid-1] + sorted_nums[mid])/2

    def longestPalindrome(self, s: str) -> str:

        start_outer_init = 0;
        end_outer_init = len(s)-1
        start_outer = start_inner = i = start_outer_init
        end_outer = end_inner = j = end_outer_init

        palindrome = "";
        longest = 0;

        while start_outer<=end_outer:
            while i<j:
                if s[i] != s[j]:
                    i +=1
                    j -= 1
                    start_inner = i
                    end_inner = j 
                else:
                    i += 1
                    j -= 1
            if end_inner-start_inner+1 > longest:
                longest = end_inner-start_inner+1
                palindrome = s[start_inner:end_inner+1]
            end_outer -= 1
            i = start_inner =start_outer
            j = end_inner = end_outer

        end_outer = end_outer_init
        while start_outer<=end_outer:
            while i<j:
                if s[i] != s[j]:
                    i +=1
                    j -= 1
                    start_inner = i
                    end_inner = j 
                else:
                    i += 1
                    j -= 1
            if end_inner-start_inner+1 > longest:
                longest = end_inner-start_inner+1
                palindrome = s[start_inner:end_inner+1]
            start_outer += 1
            i = start_inner=start_outer
            j = end_inner =end_outer


        return palindrome










