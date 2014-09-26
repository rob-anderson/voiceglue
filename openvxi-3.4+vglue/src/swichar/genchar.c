/* li_string, locale independant conversions */

/****************License************************************************
 * Vocalocity OpenVXI
 * Copyright (C) 2004-2005 by Vocalocity, Inc. All Rights Reserved.
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 * Vocalocity, the Vocalocity logo, and VocalOS are trademarks or 
 * registered trademarks of Vocalocity, Inc. 
 * OpenVXI is a trademark of Scansoft, Inc. and used under license 
 * by Vocalocity.
 ***********************************************************************/

/* -----1=0-------2=0-------3=0-------4=0-------5=0-------6=0-------7=0-------8
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "SWIchar.h"

const wchar_t *SWIcharReturnCodeToWcs( SWIcharResult rc)
{
  switch (rc) {
		case SWIchar_SUCCESS:
			return L"SUCCESS";
		case SWIchar_ERROR:
			return L"ERROR";
		case SWIchar_FAIL:
			return L"FAIL";
		case SWIchar_INVALID_INPUT:
			return L"INVALID INPUT";
		case SWIchar_OUT_OF_MEMORY:
			return L"OUT OF MEMORY";
		case SWIchar_BUFFER_OVERFLOW:
			return L"BUFFER OVERFLOW";
		case SWIchar_CONVERSION_LOSS:
			return L"CONVERSION LOSS";
		default:
			return L"UNKNOWN";
	}
}
